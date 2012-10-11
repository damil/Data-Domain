#======================================================================
package Data::Domain; # documentation at end of file
#======================================================================
use strict;
use warnings;
use Carp;
use Scalar::Does ();
use Scalar::Util ();
use Try::Tiny;
use overload '~~' => \&_matches, '""' => \&_stringify;

our $VERSION = "1.00";

our $MESSAGE; # global var for last message from ~~ (see '_matches')

#----------------------------------------------------------------------
# exports
#----------------------------------------------------------------------

my @domain_generators;
my @builtin_domains;

BEGIN {
  @domain_generators = qw/Whatever Empty
                              Num Int Date Time String
                              Enum List Struct One_of All_of/;
  @builtin_domains   = qw/True False Defined Undef Blessed Unblessed/;

}
use Sub::Exporter -setup => {
  exports => ['node_from_path', @builtin_domains,
              map {$_ => \&_wrap_domain} @domain_generators],
  groups => {
    generators => \@domain_generators,
    builtins   => \@builtin_domains,
   },
};


# convenience functions : for each builtin domain, we export a closure
# that just calls new() on the corresponding subclass. For example,
# Num(@args) is just equivalent to Data::Domain::Num->new(@args).
sub _wrap_domain {
  my ($class, $name, $args, $coll) = @_;
  return sub {return "Data::Domain::$name"->new(@_)};
}

sub True      {Data::Domain::Whatever->new(-true    => 1, @_)}
sub False     {Data::Domain::Whatever->new(-true    => 0, @_)}
sub Defined   {Data::Domain::Whatever->new(-defined => 1, @_)}
sub Undef     {Data::Domain::Whatever->new(-defined => 0, @_)}
sub Blessed   {Data::Domain::Whatever->new(-blessed => 1, @_)}
sub Unblessed {Data::Domain::Whatever->new(-blessed => 0, @_)}

#----------------------------------------------------------------------
# messages
#----------------------------------------------------------------------

my $builtin_msgs = {
  english => {
    Generic => {
      UNDEFINED     => "undefined data",
      INVALID       => "invalid",
      TOO_SMALL     => "smaller than minimum %s",
      TOO_BIG       => "bigger than maximum %s",
      EXCLUSION_SET => "belongs to exclusion set",
    },
    Whatever => {
      MATCH_DEFINED => "data defined/undefined",
      MATCH_TRUE    => "data true/false",
      MATCH_ISA     => "is not a %s",
      MATCH_CAN     => "does not have method %s",
      MATCH_DOES    => "does not have do %s",
      MATCH_BLESSED => "data blessed/unblessed",
      MATCH_SMART   => "does not smart-match %s",
    },
    Num    => {INVALID => "invalid number",},
    Date   => {INVALID => "invalid date",},
    String => {
      TOO_SHORT        => "less than %d characters",
      TOO_LONG         => "more than %d characters",
      SHOULD_MATCH     => "should match %s",
      SHOULD_NOT_MATCH => "should not match %s",
    },
    Enum => {NOT_IN_LIST => "not in enumeration list",},
    List => {
      NOT_A_LIST => "is not an arrayref",
      TOO_SHORT  => "less than %d items",
      TOO_LONG   => "more than %d its",
      ANY        => "should have at least one %s",
    },
    Struct => {
      NOT_A_HASH      => "is not a hashref",
      FORBIDDEN_FIELD => "contains forbidden field: %s"
    },
  },

  "français" => {
    Generic => {
      UNDEFINED     => "donnée non définie",
      INVALID       => "incorrect",
      TOO_SMALL     => "plus petit que le minimum %s",
      TOO_BIG       => "plus grand que le maximum %s",
      EXCLUSION_SET => "fait partie des valeurs interdites",
    },
    Whatever => {
      MATCH_DEFINED => "donnée définie/non définie",
      MATCH_TRUE    => "donnée vraie/fausse",
      MATCH_ISA     => "n'est pas un  %s",
      MATCH_CAN     => "n'a pas la méthode %s",
      MATCH_DOES    => "ne se comporte pas comme un %s",
      MATCH_BLESSED => "donnée blessed/unblessed",
      MATCH_SMART   => "n'obéit pas au smart-match %s",
    },
    Num    => {INVALID => "nombre incorrect",},
    Date   => {INVALID => "date incorrecte",},
    String => {
      TOO_SHORT        => "moins de %d caractères",
      TOO_LONG         => "plus de %d caractères",
      SHOULD_MATCH     => "devrait être reconnu par la regex %s",
      SHOULD_NOT_MATCH => "ne devrait pas être reconnu par la regex %s",
    },
    Enum => {NOT_IN_LIST => "n'appartient pas à la liste énumérée",},
    List => {
      NOT_A_LIST => "n'est pas une arrayref",
      TOO_SHORT  => "moins de %d éléments",
      TOO_LONG   => "plus de %d éléments",
      ANY        => "doit avoir au moins un %s",
    },
    Struct => {
      NOT_A_HASH      => "n'est pas une hashref",
      FORBIDDEN_FIELD => "contient le champ interdit: %s",
    },
  },
};

# inherit Int messages from Num messages
foreach my $language (keys %$builtin_msgs) {
  $builtin_msgs->{$language}{Int} = $builtin_msgs->{$language}{Num};
}

# default messages : english
my $global_msgs = $builtin_msgs->{english};

#----------------------------------------------------------------------
# PUBLIC METHODS
#----------------------------------------------------------------------

sub messages { # private class method
  my ($class, $new_messages) = @_;
  croak "messages() is a class method in Data::Domain" 
    if ref $class or $class ne 'Data::Domain';

  $global_msgs = (ref $new_messages) ? $new_messages 
                                     : $builtin_msgs->{$new_messages}
    or croak "no such builtin messages ($new_messages)";
}


sub inspect {
  my ($self, $data, $context) = @_;

  # call _inspect() if data is defined or if builtin domains Whatever
  return $self->_inspect($data, $context)
    if defined($data) or $self->isa("Data::Domain::Whatever");

  # otherwise, if data is undefined
  return if $self->{-optional};                # optional domains always succeed
  return $self->msg(UNDEFINED => '');          # otherwise : error
}

#----------------------------------------------------------------------
# METHODS FOR INTERNAL USE
#----------------------------------------------------------------------


sub msg {
  my ($self, $msg_id, @args) = @_;
  my $msgs     = $self->{-messages};
  my $subclass = $self->subclass;
  my $name     = $self->{-name} || $subclass;
  my $msg;

  # if user_defined messages
  if (defined $msgs) { 
    for (ref $msgs) {
      /^CODE/ and return $msgs->($msg_id, @args); # user function
      /^$/    and return "$name: $msgs";          # user constant string
      /^HASH/ and do { $msg =  $msgs->{$msg_id}   # user hash of msgs
                         and return sprintf "$name: $msg", @args;
                       last; # not found in this hash - revert to $global_msgs
                     };
      croak "invalid -messages option";           # otherwise
    }
  }

  # if not found, try global messages
  return $global_msgs->($msg_id, @args)      if ref $global_msgs eq 'CODE';
  $msg = $global_msgs->{$subclass}{$msg_id}  # otherwise
      || $global_msgs->{Generic}{$msg_id}
     or croak "no error string for message $msg_id";
  return sprintf "$name: $msg", @args; 
}


sub subclass {
  my ($self) = @_;
  my $class = ref($self) || $self;
  (my $subclass = $class) =~ s/^Data::Domain:://;
  return $subclass;
}


sub _expand_range {
  my ($self, $range_field, $min_field, $max_field, $ignore_check) = @_;
  my $name = $self->{-name} || $self->subclass;

  # range field will be replaced by min and max fields
  if (my $range = delete $self->{$range_field}) {
    for ($min_field, $max_field) {
      not defined $self->{$_}
        or croak  "$name: incompatible options: $range_field / $_";
    }
    Scalar::Does::does($range, 'ARRAY') and @$range == 2
        or croak  "$name: invalid argument for $range";
    @{$self}{$min_field, $max_field} = @$range;
  }

  # check numeric order of min and max bounds (unless check is disabled)
  if (!$ignore_check and defined($self->{$min_field}) 
                     and defined($self->{$max_field})) {
    $self->{$min_field} <= $self->{$max_field}
      or croak "$name: incompatible min/max values";
  }
}


sub _call_lazy_domain {
  my ($self, $domain, $context) = @_;

  if ($domain && Scalar::Does::does($domain, 'CODE')) {
    $domain = try {$domain->($context)} 
              catch {# error message without "at source_file, line ..."
                     (my $error_msg = $_) =~ s/\bat\b.*//s;
                     Data::Domain::Empty->new(-name     => "domain parameters",
                                              -messages => $error_msg);
                   };

    Scalar::Does::does($domain, "Data::Domain")
        or croak "lazy domain coderef returned an invalid domain";
    }
  return $domain;
}


#----------------------------------------------------------------------
# UTILITY FUNCTIONS (NOT METHODS) 
#----------------------------------------------------------------------

# valid options for all subclasses
my @common_options = qw/-optional -name -messages/; 

sub _parse_args {
  my ($args_ref, $options_ref, $default_option, $arg_type) = @_;

  my %parsed;

  # parse named arguments
  while (@$args_ref and $args_ref->[0] =~ /^-/) {
    grep {$args_ref->[0] eq $_} (@$options_ref, @common_options)
      or croak "invalid argument: $args_ref->[0]";
    my ($key, $val) = (shift @$args_ref, shift @$args_ref);
    $parsed{$key}  = $val;
  }

  # remaining arguments are mapped to the default option
  if (@$args_ref) {
    $default_option or croak "too many args to new()";
    not exists $parsed{$default_option}
      or croak "can't have default args if $default_option is set";
    $parsed{$default_option} 
      = $arg_type eq 'scalar'   ? $args_ref->[0]
      : $arg_type eq 'arrayref' ? $args_ref
      : croak "unknown type for default option: $arg_type";
  }

  return \%parsed;
}


sub node_from_path {
  my ($root, $path0, @path) = @_;
  return $root if not defined $path0;
  return undef if not defined $root;
  return node_from_path($root->{$path0}, @path) 
    if Scalar::Does::does($root, 'HASH');
  return node_from_path($root->[$path0], @path) 
    if Scalar::Does::does($root, 'ARRAY');

  # otherwise
  croak "node_from_path: incorrect root/path";
}


#----------------------------------------------------------------------
# overloads
#----------------------------------------------------------------------
sub _matches {
  my ($self, $data, $call_order) = @_;
  $Data::Domain::MESSAGE = $self->inspect($data);
  return !$Data::Domain::MESSAGE;
}

sub _stringify {
  my ($self) = @_;
  use Data::Dumper;
  my $dumper = Data::Dumper->new([$self])->Indent(0)->Terse(1);
  return $dumper->Dump;
}



#======================================================================
package Data::Domain::Whatever;
#======================================================================
use strict;
use warnings;
use Carp;
our @ISA = 'Data::Domain';

sub new {
  my $class   = shift;
  my @options = qw/-defined -true -isa -can -does -blessed -matches/;
  my $self    = Data::Domain::_parse_args( \@_, \@options );
  bless $self, $class;

  croak "both -defined and -optional: meaningless!"
    if $self->{-defined } and $self->{-optional};

  return $self;
}

sub _inspect {
  my ($self, $data) = @_;

  return if $self->{-optional} and not defined($data);

  if (defined $self->{-defined}) {
    return $self->msg(MATCH_DEFINED => $self->{-defined})
      if defined($data) xor $self->{-defined};
  }
  if (defined $self->{-true}) {
    return $self->msg(MATCH_TRUE => $self->{-true})
      if $data xor $self->{-true};
  }
  if (defined $self->{-isa}) {
    defined($data) && Scalar::Does::does($data, $self->{-isa})
      or return $self->msg(MATCH_ISA => $self->{-isa});
  }
  if (defined $self->{-does}) {
    defined($data) && Scalar::Does::does($data, $self->{-does})
      or return $self->msg(MATCH_DOES => $self->{-does});
  }
  if (defined $self->{-can}) {
    my $methods = ref($self->{-can}) ? $self->{-can} : [$self->{-can}];
    foreach my $method (@$methods) {
      eval {$data->can($method)}
        or return $self->msg(MATCH_CAN => $method);
    }
  }
  if (defined $self->{-blessed}) {
    return $self->msg(MATCH_BLESSED => $self->{-blessed})
      if Scalar::Util::blessed($data) xor $self->{-blessed};
  }
  if (defined $self->{-matches}) {
    $data ~~ $self->{-matches}
      or return $self->msg(MATCH_SMART => $self->{-matches});
  }
  return;    # otherwise : success
}


#======================================================================
package Data::Domain::Empty;
#======================================================================
use strict;
use warnings;
use Carp;
our @ISA = 'Data::Domain';

sub new {
  my $class   = shift;
  my @options = ();
  my $self    = Data::Domain::_parse_args( \@_, \@options );
  bless $self, $class;

  return $self;
}

sub _inspect {
  my ($self, $data) = @_;

  return $self->msg(INVALID => ''); # always fails
}


#======================================================================
package Data::Domain::Num;
#======================================================================
use strict;
use warnings;
use Carp;
use Scalar::Util qw/looks_like_number/;

our @ISA = 'Data::Domain';

sub new {
  my $class = shift;
  my @options = qw/-range -min -max -not_in/;
  my $self = Data::Domain::_parse_args(\@_, \@options);
  bless $self, $class;

  $self->_expand_range(qw/-range -min -max/);

  if ($self->{-not_in}) {
    eval {my $vals = $self->{-not_in};
          @$vals > 0 and not grep {!looks_like_number($_)} @$vals}
      or croak "-not_in : needs an arrayref of numbers";
  }

  return $self;
}

sub _inspect {
  my ($self, $data) = @_;

  looks_like_number($data) 
    or return $self->msg(INVALID => $data);

  if (defined $self->{-min}) {
    $data >= $self->{-min} 
      or return $self->msg(TOO_SMALL => $self->{-min});
  }
  if (defined $self->{-max}) {
    $data <= $self->{-max} 
      or return $self->msg(TOO_BIG => $self->{-max});
  }
  if (defined $self->{-not_in}) {
    grep {$data == $_} @{$self->{-not_in}}
      and return $self->msg(EXCLUSION_SET => $data);
  }

  return;
}


#======================================================================
package Data::Domain::Int;
#======================================================================
use strict;
use warnings;

our @ISA = 'Data::Domain::Num';

sub _inspect {
  my ($self, $data) = @_;

  defined($data) and $data =~ /^-?\d+$/
    or return $self->msg(INVALID => $data);
  return $self->SUPER::_inspect($data);
}


#======================================================================
package Data::Domain::String;
#======================================================================
use strict;
use warnings;
use Carp;
use overload;
our @ISA = 'Data::Domain';

sub new {
  my $class = shift;
  my @options = qw/-regex -antiregex
                   -range -min -max 
                   -length -min_length -max_length 
                   -not_in/;
  my $self = Data::Domain::_parse_args(\@_, \@options, -regex => 'scalar');
  bless $self, $class;

  $self->_expand_range(qw/-range -min -max don_t_check_order/);
  if ($self->{-min} and $self->{-max} and
        $self->{-min} gt $self->{-max}) {
    croak "String: incompatible min/max values";
  }

  $self->_expand_range(qw/-length -min_length -max_length/);

  return $self;
}

sub _inspect {
  my ($self, $data) = @_;

  # $data must be defined and scalar (or obj with a stringification method)
  defined($data) && (!ref($data) || overload::Method($data, '""'))
    or return $self->msg(INVALID => $data);
  if ($self->{-min_length}) {
    length($data) >= $self->{-min_length} 
      or return $self->msg(TOO_SHORT => $self->{-min_length});
  }
  if (defined $self->{-max_length}) {
    length($data) <= $self->{-max_length} 
      or return $self->msg(TOO_LONG => $self->{-max_length});
  }
  if (defined $self->{-regex}) {
    $data =~ $self->{-regex}
      or return $self->msg(SHOULD_MATCH => $self->{-regex});
  }
  if (defined $self->{-antiregex}) {
    $data !~ $self->{-antiregex}
      or return $self->msg(SHOULD_NOT_MATCH => $self->{-antiregex});
  }
  if (defined $self->{-min}) {
    $data ge $self->{-min} 
      or return $self->msg(TOO_SMALL => $self->{-min});
  }
  if (defined $self->{-max}) {
    $data le $self->{-max} 
      or return $self->msg(TOO_BIG => $self->{-max});
  }
  if (defined $self->{-not_in}) {
    grep {$data eq $_} @{$self->{-not_in}}
      and return $self->msg(EXCLUSION_SET => $data);
  }

  return; 
}



#======================================================================
package Data::Domain::Date;
#======================================================================
use strict;
use warnings;
use Carp;
our @ISA = 'Data::Domain';


use autouse 'Date::Calc' => qw/Decode_Date_EU Decode_Date_US Date_to_Text
                               Delta_Days  Add_Delta_Days Today check_date/;

my $date_parser = \&Decode_Date_EU;

#----------------------------------------------------------------------
# utility functions 
#----------------------------------------------------------------------
sub _print_date {
  my $date = shift;
  $date = _expand_dynamic_date($date);
  return Date_to_Text(@$date);
}


my $dynamic_date = qr/^(today|yesterday|tomorrow)$/;

sub _expand_dynamic_date {
  my $date = shift;
  if (not ref $date) {
    $date = {
      today     => [Today], 
      yesterday => [Add_Delta_Days(Today, -1)],
      tomorrow  => [Add_Delta_Days(Today, +1)]
     }->{$date} or croak "unexpected date : $date";
  }
  return $date;
}

sub _date_cmp {
  my ($d1, $d2) = map {_expand_dynamic_date($_)} @_;
  return -Delta_Days(@$d1, @$d2);
}


#----------------------------------------------------------------------
# public API
#----------------------------------------------------------------------

sub parser {
  my ($class, $new_parser) = @_;
  not ref $class or croak "Data::Domain::Date::parser is a class method";

  $date_parser = 
    (ref $new_parser eq 'CODE')
    ? $new_parser
    : {US => \&Decode_Date_US,
       EU => \&Decode_Date_EU}->{$new_parser}
    or croak "unknown date parser : $new_parser";
  return $date_parser;
}


sub new {
  my $class   = shift;
  my @options = qw/-range -min -max -not_in/;
  my $self    = Data::Domain::_parse_args(\@_, \@options);
  bless $self, $class;

  $self->_expand_range(qw/-range -min -max don_t_check_order/);

  # parse date boundaries into internal representation (arrayrefs)
  for my $bound (qw/-min -max/) {
    if ($self->{$bound} and $self->{$bound} !~ $dynamic_date) {
      my @date = $date_parser->($self->{$bound})
        or croak "invalid date ($bound): $self->{$bound}";
      $self->{$bound} = \@date;
    }
  }

  # check order of boundaries
  if ($self->{-min} and $self->{-max} and 
        _date_cmp($self->{-min}, $self->{-max}) > 0) {
    croak "Date: incompatible min/max values";
  }

  # parse dates in the exclusion set into internal representation
  if ($self->{-not_in}) {
    my @excl_dates;
    eval {
      foreach my $date (@{$self->{-not_in}})
      {
        if ($date =~ $dynamic_date) {
          push @excl_dates, $date;
        }
        else {
          my @parsed_date = $date_parser->($date) or die "wrong date";
          push @excl_dates, \@parsed_date;
        }
      }
      @excl_dates > 0;
    }
      or croak "-not_in : needs an arrayref of dates";
    $self->{-not_in} = \@excl_dates;
  }

  return $self;
}


sub _inspect {
  my ($self, $data) = @_;

  my @date = eval {$date_parser->($data)};
  @date && check_date(@date)
    or return $self->msg(INVALID => $data);

  if (defined $self->{-min}) {
    my $min = _expand_dynamic_date($self->{-min});
    !check_date(@$min) || (_date_cmp(\@date, $min) < 0)
      and return $self->msg(TOO_SMALL => _print_date($self->{-min}));
  }

  if (defined $self->{-max}) {
    my $max = _expand_dynamic_date($self->{-max});
    !check_date(@$max) || (_date_cmp(\@date, $max) > 0)
      and return $self->msg(TOO_BIG => _print_date($self->{-max}));
  }

  if (defined $self->{-not_in}) {
    grep {_date_cmp(\@date, $_) == 0} @{$self->{-not_in}}
      and return $self->msg(EXCLUSION_SET => $data);
  }

  return;
}


#======================================================================
package Data::Domain::Time;
#======================================================================
use strict;
use warnings;
use Carp;
our @ISA = 'Data::Domain';

my $time_regex = qr/^(\d\d?):?(\d\d?)?:?(\d\d?)?$/;

sub _valid_time {
  my ($h, $m, $s) = @_;
  $m ||= 0;
  $s ||= 0;
  return ($h <= 23 && $m <= 59 && $s <= 59);
}


sub _expand_dynamic_time {
  my $time = shift;
  if (not ref $time) {
    $time eq 'now' or croak "unexpected time : $time";
    $time = [(localtime)[2, 1, 0]];
  }
  return $time;
}


sub _time_cmp {
  my ($t1, $t2) = map {_expand_dynamic_time($_)} @_;

  return  $t1->[0]       <=>  $t2->[0]        # hours
      || ($t1->[1] || 0) <=> ($t2->[1] || 0)  # minutes
      || ($t1->[2] || 0) <=> ($t2->[2] || 0); # seconds
}

sub _print_time {
  my $time = _expand_dynamic_time(shift);
  return sprintf "%02d:%02d:%02d", $time->[0], $time->[1]||0, $time->[2]||0;
}


sub new {
  my $class = shift;
  my @options = qw/-range -min -max/;
  my $self = Data::Domain::_parse_args(\@_, \@options);
  bless $self, $class;

  $self->_expand_range(qw/-range -min -max don_t_check_order/);

  # parse time boundaries
  for my $bound (qw/-min -max/) {
    if ($self->{$bound} and $self->{$bound} ne 'now') {
      my @time = ($self->{$bound} =~ $time_regex);
      @time && _valid_time(@time)
        or croak "invalid time ($bound): $self->{$bound}";
      $self->{$bound} = \@time;
    }
  }

  # check order of boundaries
  if ($self->{-min} and $self->{-max} and 
        _time_cmp($self->{-min}, $self->{-max}) > 0) {
    croak "Time: incompatible min/max values";
  }

  return $self;
}


sub _inspect {
  my ($self, $data) = @_;

  my @t = ($data =~ $time_regex);
  @t and _valid_time(@t)
    or return $self->msg(INVALID => $data);

  if (defined $self->{-min}) {
    _time_cmp(\@t, $self->{-min}) < 0
      and return $self->msg(TOO_SMALL => _print_time($self->{-min}));
  }

  if (defined $self->{-max}) {
    _time_cmp(\@t, $self->{-max}) > 0
      and return $self->msg(TOO_BIG => _print_time($self->{-max}));
  }

  return;
}



#======================================================================
package Data::Domain::Enum;
#======================================================================
use strict;
use warnings;
use Carp;
our @ISA = 'Data::Domain';

sub new {
  my $class = shift;
  my @options = qw/-values/;
  my $self = Data::Domain::_parse_args(\@_, \@options, -values => 'arrayref');
  bless $self, $class;

  eval {@{$self->{-values}}} or croak "Enum : incorrect set of values";

  not grep {! defined $_} @{$self->{-values}}
    or croak "Enum : undefined element in values";

  return $self;
}


sub _inspect {
  my ($self, $data) = @_;

  return $self->msg(NOT_IN_LIST => $data)
    if not grep {$_ eq $data} @{$self->{-values}};

  return; # otherwise OK, no error
}


#======================================================================
package Data::Domain::List;
#======================================================================
use strict;
use warnings;
use Carp;
use List::Util 'max';
our @ISA = 'Data::Domain';

sub new {
  my $class = shift;
  my @options = qw/-items -size -min_size -max_size -any -all/;
  my $self = Data::Domain::_parse_args(\@_, \@options, -items => 'arrayref');
  bless $self, $class;

  $self->_expand_range(qw/-size -min_size -max_size/);

  if ($self->{-items}) {
    Scalar::Does::does($self->{-items}, 'ARRAY')
      or croak "invalid -items for Data::Domain::List";

    # if -items is given, then both -{min,max}_size cannot be shorter
    for my $bound (qw/-min_size -max_size/) {
      croak "$bound does not match -items"
      if $self->{$bound} and $self->{$bound} < @{$self->{-items}};
    }
  }

  return $self;
}


sub _inspect {
  my ($self, $data, $context) = @_;

  defined($data) && Scalar::Does::does($data, 'ARRAY')
    or return $self->msg(NOT_A_LIST => $data);

  if (defined $self->{-min_size} && @$data < $self->{-min_size}) {
    return $self->msg(TOO_SHORT => $self->{-min_size});
  }

  if (defined $self->{-max_size} && @$data > $self->{-max_size}) {
    return $self->msg(TOO_LONG => $self->{-max_size});
  }

  return unless $self->{-items} || $self->{-all} || $self->{-any};

  # prepare context for calling lazy subdomains
  $context ||= {root => $data,
                flat => {},
                path => []};

  local $context->{list} = $data;
  my @msgs;
  my $has_invalid;

  my $items   = $self->{-items} || [];
  my $n_items = @$items;
  my $n_data  = @$data;

  for (my $i = 0; $i < $n_items; $i++) {
    local $context->{path} = [@{$context->{path}}, $i];
    my $subdomain  = $self->_call_lazy_domain($items->[$i], $context)
      or next;
    $msgs[$i]      = $subdomain->inspect($data->[$i], $context);
    $has_invalid ||= $msgs[$i];
  }

  if ($self->{-all}) {
    for (my $i = $n_items; $i < $n_data; $i++) {
      local $context->{path} = [@{$context->{path}}, $i];
      my $subdomain  = $self->_call_lazy_domain($self->{-all}, $context);
      $msgs[$i]      = $subdomain->inspect($data->[$i], $context);
      $has_invalid ||= $msgs[$i];
    }
  }

  return \@msgs if $has_invalid; 

  # all other conditions were good, now check the "any" conditions
  my @anys = $self->{-any} 
                ? Scalar::Does::does($self->{-any}, 'ARRAY') ? @{$self->{-any}}
                                                             : ($self->{-any})
                : ();


  # if there is an 'any' condition, there must be data to inspect
  return $self->msg(ANY => ($anys[0]{-name} || $anys[0]->subclass))
      if @anys and not $n_data > $n_items;

  # inspect the remaining data for all 'any' conditions
 ANYS:
  foreach my $any (@anys) {
    my $subdomain;
    for (my $i = $n_items; $i < $n_data; $i++) {
      local $context->{path} = [@{$context->{path}}, $i];
         $subdomain = $self->_call_lazy_domain($any, $context);
      my $error     = $subdomain->inspect($data->[$i], $context);
      next ANYS if not $error;
    }
    return $self->msg(ANY => ($subdomain->{-name} || $subdomain->subclass));
  }

  return; # OK, no error
}


#======================================================================
package Data::Domain::Struct;
#======================================================================
use strict;
use warnings;
use Carp;
our @ISA = 'Data::Domain';

# TODO: somehow keep the order of field names

sub new {
  my $class = shift;
  my @options = qw/-fields -exclude/;
  my $self = Data::Domain::_parse_args(\@_, \@options, -fields => 'arrayref');
  bless $self, $class;

  my $fields = $self->{-fields} || [];
  for (ref $fields) {

    # transform arrayref into hashref plus an ordered list of keys
    /^ARRAY/ and do { 
      $self->{-fields_list} = [];
      $self->{-fields}      = {};
      for (my $i = 0; $i < @$fields; $i += 2) {
        my ($key, $val) = ($fields->[$i], $fields->[$i+1]);
        push @{$self->{-fields_list}}, $key;
        $self->{-fields}{$key} = $val;
      }
      last;
    };

    # keep given hashref, add list of keys
    /^HASH/ and do {
      $self->{-fields_list} = [keys %$fields];
      last;
    };

    # otherwise
    croak "invalid data for -fields option";
  }

  croak "invalid data for -exclude option" 
    if $self->{-exclude} and ref($self->{-exclude}) !~ /^(ARRAY|Regexp|)$/;

  return $self;
}


sub _inspect {
  my ($self, $data, $context) = @_;

  # check that $data is a hashref
  defined($data) && Scalar::Does::does($data, 'HASH')
    or return $self->msg(NOT_A_HASH => $data);

  # check if there are any forbidden fields
  if (defined $self->{-exclude}) {
    foreach my $field (keys %$data) {
      next if $self->{-fields}{$field};

      my $ref = ref $self->{-exclude};
      return $self->msg(FORBIDDEN_FIELD => $field)
        if $ref eq 'ARRAY'  and grep {$field eq $_} @{$self->{-exclude}}
        or $ref eq 'Regexp' and $field =~ $self->{-exclude}
        or $ref eq ''       and $self->{-exclude} =~ /\*|all/;
    }
  }

  my %msgs;
  my $has_invalid;

  # prepare context for calling lazy subdomains
  $context ||= {root => $data,
                flat => {},
                list => [],
                path => []};
  local $context->{flat} = {%{$context->{flat}}, %$data};

  # check fields of the domain
  foreach my $field (@{$self->{-fields_list}}) {
    local $context->{path} = [@{$context->{path}}, $field];
    my $field_spec = $self->{-fields}{$field};
    my $subdomain  = $self->_call_lazy_domain($field_spec, $context);
    my $msg        = $subdomain->inspect($data->{$field}, $context);
    $msgs{$field}  = $msg if $msg;
    $has_invalid ||=  $msg;
  }

  return $has_invalid ? \%msgs : undef;
}

#======================================================================
package Data::Domain::One_of;
#======================================================================
use strict;
use warnings;
use Carp;
our @ISA = 'Data::Domain';

sub new {
  my $class = shift;
  my @options = qw/-options/;
  my $self = Data::Domain::_parse_args(\@_, \@options, -options => 'arrayref');
  bless $self, $class;

  $self->{-options} and Scalar::Does::does($self->{-options}, 'ARRAY')
    or croak "One_of: invalid options";

  return $self;
}


sub _inspect {
  my ($self, $data, $context) = @_;
  my @msgs;

  for my $subdomain (@{$self->{-options}}) {
    my $msg = $subdomain->inspect($data, $context)
      or return; # $subdomain was successful
    push @msgs, $msg;
  }
  return \@msgs;
}


#======================================================================
package Data::Domain::All_of;
#======================================================================
use strict;
use warnings;
use Carp;
our @ISA = 'Data::Domain';

sub new {
  my $class = shift;
  my @options = qw/-options/;
  my $self = Data::Domain::_parse_args(\@_, \@options, -options => 'arrayref');
  bless $self, $class;

  $self->{-options} and Scalar::Does::does($self->{-options}, 'ARRAY')
    or croak "All_of: invalid options";

  return $self;
}


sub _inspect {
  my ($self, $data, $context) = @_;
  my @msgs;

  for my $subdomain (@{$self->{-options}}) {
    my $msg = $subdomain->inspect($data, $context);
    push @msgs, $msg if $msg; # subdomain failed
  }
  return @msgs ? \@msgs : undef;
}


#======================================================================
1;


__END__

=encoding ISO8859-1

=head1 NAME

Data::Domain - Data description and validation

=head1 SYNOPSIS

  use Data::Domain qw/:all/;

  my $domain = Struct(
    anInt      => Int(-min => 3, -max => 18),
    aNum       => Num(-min => 3.33, -max => 18.5),
    aDate      => Date(-max => 'today'),
    aLaterDate => sub {my $context = shift;
                       Date(-min => $context->{flat}{aDate})},
    aString    => String(-min_length => 2, -optional => 1),
    anEnum     => Enum(qw/foo bar buz/),
    anIntList  => List(-min_size => 1, -all => Int),
    aMixedList => List(Integer, String, Int(-min => 0), Date, True, Defined),
    aStruct    => Struct(foo => String, bar => Int(-optional => 1))
  );

  my $messages = $domain->inspect($some_data);
  display_error($messages) if $messages;

  # smart match API
  $some_other_data ~~ $domain
    or die "did not match because $Data::Domain::MESSAGE";

  # custom name and custom messages (2 different ways)
  $domain = Int(-name => 'age', -min => 3, -max => 18, 
                -messages => "only for people aged 3-18");
  $domain = Int(-name => 'age', -min => 3, -max => 18, -messages => {
                   TOO_BIG   => "not for old people over %d",
                   TOO_SMALL => "not for babies under %d",
                 });

  # recursive domain
  my $expr_domain = One_of(Num, Struct(operator => String(qr(^[-+*/]$)),
                                       left     => sub {$expr_domain},
                                       right    => sub {$expr_domain}));


=head1 DESCRIPTION

A data domain is a description of a set of values, either
scalar or structured (arrays or hashes).
The description can include many constraints, like minimal or maximal
values, regular expressions, required fields, forbidden fields, and
also contextual dependencies. From that description, one can then
invoke the domain's C<inspect> method to check if a given value belongs 
to it or not. In case of mismatch, a structured set of error messages is returned.

The motivation for writing this package was to be able to express in a
compact way some possibly complex constraints about structured
data. Typically the data is a Perl tree (nested hashrefs or arrayrefs)
that may come from XML, L<JSON|JSON>, from a database through
L<DBIx::DataModel|DBIx::DataModel>, or from postprocessing an HTML
form through L<CGI::Expand|CGI::Expand>. C<Data::Domain> is a kind of
tree parser on that structure, with some facilities for dealing with
dependencies within the structure, and with several options to 
finely tune the error messages returned to the user.

There are several other packages in CPAN doing data validation; these
are briefly listed in the L</"SEE ALSO"> section.


=head1 GLOBAL API

=head2 Shortcut functions for domain constructors

Internally, domains are represented as Perl objects; however, it would
be tedious to write

  my $domain = Data::Domain::Struct->new(
    anInt      => Data::Domain::Int->new(-min => 3, -max => 18),
    aDate      => Data::Domain::Date->new(-max => 'today'),
    ...
  );

so for each of its builtin domain constructors, C<Data::Domain>
exports a plain function that just calls C<new> on the appropriate 
subclass. If you import those functions (C<use Data::Domain qw/:all/>,
or C<use Data::Domain qw/:generators/>,
or C<use Data::Domain qw/Struct Int Date .../>),
then you can write more conveniently :

  my $domain = Struct(
    anInt      => Int(-min => 3, -max => 18),
    aDate      => Date(-max => 'today'),
    ...
  );

=head2 Builtin domains

C<Data::Domain> also exports some shortcuts for builtin domains;
these are imported through C<use Data::Domain qw/:builtins/>
or C<use Data::Domain qw/:all/> : 

=over

=item True

=item False

=item Defined

=item Undef

=item Blessed

=item Unblessed

=back

and correspond to 
C<< Whatever(-true => 1) >>, 
C<< Whatever(-true => 0) >>, etc. 
(see the L</Whatever> class below).


=head2 Renaming imported functions

Short function names like C<Int> or C<String> are convenient, but 
may cause name clashes with other modules. However, thanks to the 
powerful features of L<Sub::Exporter>, these functions
can be renamed in various ways. Here is an example :

  use Data::Domain -all => { -prefix => 'dom_' };
  my $domain = dom_Struct(
    anInt      => dom_Int(-min => 3, -max => 18),
    aDate      => dom_Date(-max => 'today'),
    ...
  );

See L<Sub::Exporter> for other renaming examples.


=head2 Smart matching

C<Data::Domain> overloads the smart match operator C<~~>,
so one can write 

  if ($data ~~ $domain) {...}

instead of 

  if (!my $msg = $domain->inspect($data)) {...}

The error message from the last smart match operation can be
retrieved from C<$Data::Domain::MESSAGE>.

=head2 Methods

=head3 new

Creates a new domain object, from one of the domain constructors
listed below (C<Num>, C<Int>, C<Date>, etc.). 
The C<Data::Domain> class itself has no
C<new> method, because it is an abstract class.

Arguments to the C<new> method specify various constraints for the
domain (minimal/maximal values, regular expressions, etc.); most often
they are specific to a given domain constructor, so see the details
below. However, there are also some generic options :

=over

=item C<-optional>

if true, an C<undef> value will be accepted, without generating an
error message

=item C<-name>

defines a name for the domain, that will be printed in error
messages instead of the subclass name. 

=item C<-messages>

defines ad hoc messages for that domain, instead of the builtin
messages. The argument can be a string, a hashref or a coderef,
as explained in the  L</"ERROR MESSAGES"> section.


=back

Option names always start with a dash. If no option name is given,
parameters to the C<new> method are passed to the I<default option>,
as defined in each constructor subclass. For example
the default option in  C<Data::Domain::List> is C<-items>, so 

   my $domain = List(Int, String, Int);

is equivalent to

   my $domain = List(-items => [Int, String, Int]);


=head3 inspect

  my $messages = $domain->inspect($some_data);

Inspects the supplied data, and returns an error message
(or a structured collection of messages) if anything is wrong.
If the data successfully passed all domain tests, then nothing
is returned.

For scalar domains (C<Num>, C<String>, etc.), the error message
is just a string. For structured domains (C<List>, C<Struct>),
the return value is an arrayref or hashref of the same structure, like
for example

  {anInt => "smaller than mimimum 3",
   aDate => "not a valid date",
   aList => ["message for item 0", undef, undef, "message for item 3"]}

The client code can then exploit this structure to dispatch 
error messages to appropriate locations (typically these
will be the form fields that gathered the data).




=head1 BUILTIN DOMAIN CONSTRUCTORS

B<Note> : each builtin domaine described in this chapter has a set of
specific options, in addition to the generic options C<-optional>,
C<-name> and C<-messages> mentioned above in method L</new>.
Besides, some domains have a I<default option> which is syntactic sugar
for using positional parameters instead of named parameters; see the
L</String> and L</List> examples.

=head2 Whatever

  my $domain = Struct(
    just_anything => Whatever,
    is_defined    => Whatever(-defined => 1),
    is_undef      => Whatever(-defined => 0),
    is_true       => Whatever(-true => 1),
    is_false      => Whatever(-true => 0),
    is_of_class   => Whatever(-isa  => 'Some::Class'),
    does_role     => Whatever(-does => 'Some::Role'),
    has_methods   => Whatever(-can  => [qw/jump swim dance sing/]),
  );

Encapsulates just any kind of Perl value (including C<undef>). 
Options are : 

=over

=item -defined

If true, the data must be defined. If false, the data must be undef.

=item -true

If true, the data must be true. If false, the data must be false.

=item -isa

The data must be an object of the specified class.

=item -can

The data must implement the listed methods, supplied either
as an arrayref (several methods) or as a scalar (just one method).

=item -does

The data must "do" the supplied role; this is decided
through L<Scalar::Does>.

=back


=head2 Empty

Empty domain, that always fails when inspecting any data.
This is sometimes useful within lazy constructors (see below),
like in this example :

  Struct(
    foo => String,
    bar => sub {
      my $context = shift;
      if (some_condition($context)) { 
        return Empty(-messages => 'your data is wrong')
      }
      else {
        ...
      }
    }
  )


=head2 Num

  my $domain = Num(-range =>[-3.33, 999], -not_in => [2, 3, 5, 7, 11]);

Domain for numbers (including floats). Numbers are 
recognized through L<Scalar::Util/looks_like_number>.
Options for the domain are :

=over

=item -min

The data must be greater or equal to the supplied value.

=item -max

The data must be smaller or equal to the supplied value.

=item -range

C<< -range => [$min, $max] >> is equivalent to 
C<< -min => $min, -max => $max >>.

=item -not_in

The data must be different from all values in the exclusion set,
supplied as an arrayref.

=back




=head2 Int

  my $domain = Int(-min => 0, -max => 999, -not_in => [2, 3, 5, 7, 11]);

Domain for integers. Integers are 
recognized through the regular expression C</^-?\d+$/>.
This domain accepts the same options as C<Num> and returns the 
same error messages.


=head2 Date

  Data::Domain::Date->parser('EU'); # default    
  my $domain = Date(-min => '01.01.2001', 
                    -max => 'today',
                    -not_in => ['02.02.2002', '03.03.2003', 'yesterday']);

Domain for dates, implemented via the 
L<Date::Calc|Date::Calc> module. 
By default, dates are parsed according to the 
european format, i.e. through the 
L<Decode_Date_EU|Date::Calc/Decode_Date_EU> method;
this can be changed by setting 

  Data::Domain::Date->parser('US'); # will use Decode_Date_US

or 

  Data::Domain::Date->parser(\&your_own_date_parsing_function);
  # that func. should return an array ($year, $month, $day)

When outputting error messages, dates will be printed 
according to L<Date::Calc|Date::Calc>'s current language (english
by default); see that module's documentation for changing 
the language.

In the options below, the special keywords C<today>, 
C<yesterday> or C<tomorrow> may be used instead of a date
constant, and will be replaced by the appropriate date
when performing comparisons.

=over

=item -min

The data must be greater or equal to the supplied value.

=item -max

The data must be smaller or equal to the supplied value.

=item -range

C<< -range => [$min, $max] >> is equivalent to 
C<< -min => $min, -max => $max >>.

=item -not_in

The data must be different from all values in the exclusion set,
supplied as an arrayref.

=back



=head2 Time

  my $domain = Time(-min => '08:00', -max => 'now');

Domain for times in format C<hh:mm:ss> (minutes and seconds are optional).

In the options below, the special keyword C<now> may be used instead of a 
time, and will be replaced by the current local time
when performing comparisons.

=over

=item -min

The data must be greater or equal to the supplied value.

=item -max

The data must be smaller or equal to the supplied value.

=item -range

C<< -range => [$min, $max] >> is equivalent to 
C<< -min => $min, -max => $max >>.

=back



=head2 String

  my $domain = String(qr/^[A-Za-z0-9_\s]+$/);

  my $domain = String(-regex     => qr/^[A-Za-z0-9_\s]+$/,
                      -antiregex => qr/$RE{profanity}/,    # see Regexp::Common
                      -range     => ['AA', 'zz'],
                      -length    => [1, 20],
                      -not_in    => [qw/foo bar/]);

Domain for strings. Options are:


=over

=item -regex

The data must match the supplied compiled regular expression. 
Don't forget to put C<^> and C<$> anchors if you want your regex to check 
the whole string.

C<-regex> is the default option, so you may just pass the regex as a single
unnamed argument to C<String()>.

=item -antiregex

The data must not match the supplied regex.

=item -min

The data must be greater or equal to the supplied value.

=item -max

The data must be smaller or equal to the supplied value.

=item -range

C<< -range => [$min, $max] >> is equivalent to 
C<< -min => $min, -max => $max >>.

=item -min_length

The string length must be greater or equal to the supplied value.

=item -max_length

The string length must be smaller or equal to the supplied value.

=item -length

C<< -length => [$min, $max] >> is equivalent to 
C<< -min_length => $min, -max_length => $max >>.


=item -not_in

The data must be different from all values in the exclusion set,
supplied as an arrayref.

=back



=head2 Enum

  my $domain = Enum(qw/foo bar buz/);

Domain for a finite set of scalar values.
Options are:


=over

=item -values

Ref to an array of values admitted in the domain. 
This would be called as C<< Enum(-values => [qw/foo bar buz/]) >>,
but since this it is the default option, it can be
simply written as C<< Enum(qw/foo bar buz/) >>.

Undefined values are not allowed in the list (use
the C<-optional> argument instead).

=back



=head2 List

  my $domain = List(String, Int, String, Num);

  my $domain = List(-items => [String, Int, String, Num]); # same as above

  my $domain = List(-all  => String(qr/^[A-Z]+$/),
                    -any  => String(-min_length => 3),
                    -size => [3, 10]);


Domain for lists of values (stored as Perl arrayrefs).
Options are:

=over

=item -items

Ref to an array of domains; then the first I<n> items in the data must
match those domains, in the same order.

This is the default option, so item domains may be passed directly
to the C<new> method, without the C<-items> keyword.

=item -min_size

The data must be a ref to an array with at least that number of entries.

=item -max_size

The data must be a ref to an array with at most that number of entries.

=item -size

C<< -size => [$min, $max] >> is equivalent to 
C<< -min_size => $min, -max_size => $max >>.


=item -all

All remaining entries in the array, after the first <n> entries
as specified by the C<-items> option (if any), must satisfy that
domain specification.

=item -any

At least one remaining entry in the array, after the first I<n> entries
as specified by the C<-items> option (if any), must satisfy that
domain specification. A list domain can have both an C<-all> and
and C<-any> constraint.

The argument to C<-any> can also be an arrayref of domains, as in

   List(-any => [String(qr/^foo/), Num(-range => [1, 10]) ])

This means that one member of the list must be a string
starting with C<foo>, and one member of the list (in this case,
necessarily another one) must be a number between 1 and 10.
Note that this is different from 

   List(-any => One_of(String(qr/^foo/), Num(-range => [1, 10]))

which says that one member of the list must be I<either>
a string starting with C<foo> I<or> a number between 1 and 10.

=back


=head2 Struct

  my $domain = Struct(foo => Int, bar => String);

  my $domain = Struct(-fields  => [foo => Int, bar => String],
                      -exclude => '*');

Domain for associative structures (stored as Perl hashrefs).
Options are:

=over

=item -fields

Supplies a list of keys with their associated domains. The list might
be given either as a hashref or as an arrayref. 
Specifying it as an arrayref is useful for controlling 
the order in which field checks will be performed;
this may make a difference when there are context dependencies (see 
L<"LAZY CONSTRUCTORS"|/"LAZY CONSTRUCTORS (CONTEXT DEPENDENCIES)"> below ).


=item -exclude

Specifies which keys are not allowed in the structure. The exclusion 
may be specified as an arrayref of key names, as a compiled regular
expression, or as the string constant 'C<*>' or 'C<all>' (meaning
that no key will be allowed except those explicitly listed in the 
C<-fields> option.

=back




=head2 One_of

  my $domain = One_of($domain1, $domain2, ...);

Union of domains : successively checks the member domains,
until one of them succeeds. 
Options are:


=over

=item -options

List of domains to be checked. This is the default option, so 
the keyword may be omitted.

=back


=head2 All_of

  my $domain = All_of($domain1, $domain2, ...);

Intersection of domains : checks all member domains,
and requires that all of them succeed. Options are:


=over

=item -options

List of domains to be checked. This is the default option, so 
the keyword may be omitted.

=back



=head1 LAZY CONSTRUCTORS (CONTEXT DEPENDENCIES)

=head2 Principle

If an element of a structured domain (C<List> or C<Struct>) depends on 
another element, then we need to I<lazily> construct the domain.
Consider for example a struct in which the value of field C<date_end> 
must be greater than C<date_begin> : 
the subdomain for C<date_end> can only be constructed 
when the argument to C<-min> is known, namely when
the domain inspects an actual data structure.

Lazy domain construction is achieved by supplying a subroutine reference
instead of a domain object. That subroutine will be called with some
I<context> information, and should return the domain object.
So our example becomes :

  my $domain = Struct(
       date_begin => Date,
       date_end   => sub {my $context = shift;
                          Date(-min => $context->{flat}{date_begin})}
     );

=head2 Structure of context

The supplied context is a hashref containing the following information:

=over

=item root

the overall root of the inspected data 

=item path

the sequence of keys or array indices that led to the current
data node. With that information, the subdomain is able to jump
to other ancestor or sibling data nodes within the tree, with
help of the L<node_from_path> function.

=item flat

a flat hash containing an entry for any hash key met so far while
traversing the tree. In case of name clashes, most recent keys 
(down in the tree) override previous keys. 

=item list

a reference to the last list (arrayref) encountered
while traversing the tree.


=back

Here is an example :

  my $data   = {foo => [undef, 99, {bar => "hello, world"}]};
  my $domain = Struct(
     foo => List(Whatever, 
                 Whatever, 
                 Struct(bar => sub {my $context = shift;
                                    print Dumper($context);
                                    String;})
                )
     );
  $domain->inspect($data);

This code will print something like

  $VAR1 = {
    'root' => {'foo' => [undef, 99, {'bar' => 'hello, world'}]},
    'path' => ['foo', 2, 'bar'],
    'list' => $VAR1->{'root'}{'foo'},
    'flat' => {
      'bar' => 'hello, world',
      'foo' => $VAR1->{'root'}{'foo'}
    }
  };


=head2 Usage examples

=head3 Contextual sets

  my $some_cities = {
     Switzerland => [qw/Genève Lausanne Bern Zurich Bellinzona/],
     France      => [qw/Paris Lyon Marseille Lille Strasbourg/],
     Italy       => [qw/Milano Genova Livorno Roma Venezia/],
  };
  my $domain = Struct(
     country => Enum(keys %$some_cities),
     city    => sub {
        my $context = shift;
        Enum(-values => $some_cities->{$context->{flat}{country}});
      });


=head3 Ordered lists

Here is an example of a domain for ordered lists of integers:

  my $domain = List(-all => sub {
      my $context = shift;
      my $index = $context->{path}[-1];
      return Int if $index == 0; # first item has no constraint
      return Int(-min => $context->{list}[$index-1] + 1);
    });


=head3 Recursive domains

A domain for expression trees, where leaves are numbers,
and intermediate nodes are binary operators on subtrees

  my $expr_domain = One_of(Num, Struct(operator => String(qr(^[-+*/]$)),
                                       left     => sub {$expr_domain},
                                       right    => sub {$expr_domain}));


=head1 WRITING NEW DOMAIN CONSTRUCTORS

Implementing new domain constructors is fairly simple : create
a subclass of C<Data::Domain> and implement a C<new> method and
an C<_inspect> method. See the source code of C<Data::Domain::Num> or 
C<Data::Domain::String> for short examples.

However, before writing such a class, consider whether the existing
mechanisms are not enough for your needs. For example, many domains
could be expressed as a C<String> constrained by a regular
expression; therefore it is just a matter of writing a wrapper that
supplies that regular expression, and passes other arguments (like
C<-optional>) to the C<String> constructor :

  sub Phone   { String(-regex    => qr/^\+?[0-9() ]+$/, 
                       -messages => "Invalid phone number", @_) }
  sub Email   { String(-regex    => qr/^[-.\w]+\@[\w.]+$/,
                       -messages => "Invalid email", @_) }
  sub Contact { Struct(-fields => [name   => String,
                                   phone  => Phone,
                                   mobile => Phone(-optional => 1),
                                   emails => List(-all => Email)   ], @_) }


=head1 ERROR MESSAGES

Messages returned by validation rules have default values, 
but can be customized in several ways.

Each error message has an internal string identifier, like
C<TOO_SHORT>, C<NOT_A_HASH>, etc. The documentation for each builtin
domain tells which message identifiers may be generated in that
domain.  Message identifiers are then associated with user-friendly
strings, either within the domain itself, or via a global table.
Such strings are actually L<sprintf|perlfunc/sprintf>
format strings, with placeholders for printing some specific
details about the validation rule : for example the C<String>
domain defines default messages such as 

      TOO_SHORT        => "less than %d characters",
      SHOULD_MATCH     => "should match %s",

=head2 The C<-messages> option to domain constructors

Any domain constructor may receive a 
C<-messages> option to locally override the 
messages for that domain. The argument may be

=over

=item *

a plain string : that string will be returned for any kind of 
validation error within the domain

=item *

a hashref : keys of the hash should be message identifiers, and
values should be the associated error strings.

=item *

a coderef : the referenced subroutine is called, and the return
value becomes the error string. The called subroutine receives
the message identifier as argument.

=back

Here is an example :

 sub Phone { 
   String(-regex      => qr/^\+?[0-9() ]+$/, 
          -min_length => 7,
          -messages   => {
            TOO_SHORT    => "phone number should have at least %d digits",
            SHOULD_MATCH => "invalid chars in phone number"
           }, @_) 
 }




=head2 The C<messages> class method

Default strings associated with message identifiers are stored in a
global table. The distribution contains builtin tables for english
(the default) and for french : these can be chosen through the
C<messages> class method :

  Data::Domain->messages('english');  # the default
  Data::Domain->messages('français');

The same method can also receive  a custom table.

  my $custom_table = {...};
  Data::Domain->messages($custom_table);

This should be a two-level hashref : first-level entries in the hash
correspond to C<Data::Domain> subclasses (i.e C<< Num => {...} >>, C<<
String => {...} >>), or to the constant C<Generic>; for each of those,
the second-level entries should correspond to message identifiers as
specified in the doc for each subclass (for example C<TOO_SHORT>,
C<NOT_A_HASH>, etc.).  Values should be strings suitable to be fed to
L<sprintf>.  Look at C<$builtin_msgs> in the source code to see an
example.

Finally, it is also possible to write your own message generation 
handler : 

  Data::Domain->messages(sub {my ($msg_id, @args) = @_;
                              return "you just got it wrong ($msg_id)"});

What is received in 
C<@args> depends on which validation rule is involved;
it can be for example the minimal or maximal bounds,
or the regular expression being checked.

=head2 The C<-name> option to domain constructors

The name of the domain is prepended in front of error 
messages. The default name is the subclass of C<Data::Domain>, 
so a typical error message for a string would be 

  String: less than 7 characters

However, if a C<-name> is supplied to the domain constructor,
that name will be printed instead;

  my $dom = String(-min_length => 7, -name => 'Phone');
  # now error would be: "Phone: less than 7 characters"


=head2 Message identifiers

This section lists all possible message identifiers generated
by the builtin constructors.

=over

=item C<Whatever>

C<MATCH_DEFINED>, C<MATCH_TRUE>, C<MATCH_ISA>, C<MATCH_CAN>,
C<MATCH_DOES>, C<MATCH_BLESSED>, C<MATCH_SMART>.

=item C<Num>

C<INVALID>, C<TOO_SMALL>, C<TOO_BIG>, C<EXCLUSION_SET>.

=item C<Date>

C<INVALID>, C<TOO_SMALL>, C<TOO_BIG>, C<EXCLUSION_SET>.


=item C<Time>

C<INVALID>, C<TOO_SMALL>, C<TOO_BIG>.


=item C<String>

C<TOO_SHORT>, C<TOO_LONG>, C<TOO_SMALL>, C<TOO_BIG>,
C<EXCLUSION_SET>, C<SHOULD_MATCH>, C<SHOULD_NOT_MATCH>.

=item C<Enum>

C<NOT_IN_LIST>.

=item C<List>

The domain will first check if the supplied array is of appropriate
shape; in case of of failure, it will return of the following scalar
messages :  C<NOT_A_LIST>, C<TOO_SHORT>, C<TOO_LONG>.

Then it will check all items in the supplied array according to 
the C<-items> and C<-all> specifications; in case of failure,
an arrayref of messages is returned, where message positions correspond 
to the positions of offending data items.

Finally, the domain will check the C<-any> constraint; in 
case of failure, it returns an C<ANY> scalar message.
Since that message contains the name of the missing domain,
it is a good idea to use the C<-name> option so that the 
message is easily comprehensible, as for example in 

  List(-any => String(-name => "uppercase word", 
                      -regex => qr/^[A-Z]$/))

Here the error message would be : I<should have at least one uppercase word>.


=item C<Struct>

The domain will first check if the supplied hash is of appropriate
shape; in case of of failure, it will return of the following scalar
messages :  C<NOT_A_HASH>, C<FORBIDDEN_FIELD>.

Then it will check all entries in the supplied hash according to 
the C<-fields> specification, and return a
hashref of messages, where keys correspond to the
keys of offending data items.

=item C<One_of>

If all member domains failed to accept the data, an arrayref
or error messages is returned, where the order of messages
corresponds to the order of the checked domains.


=back



=head1 INTERNALS

=head2 node_from_path

  my $node = node_from_path($root, @path);

Convenience function to find a given node in a data tree, starting
from the root and following a I<path> (a sequence of hash keys or
array indices). Returns C<undef> if no such path exists in the tree.
Mainly useful for contextual constraints in lazy constructors.

=head2 msg

Internal utility method for generating an error message.

=head2 subclass

Method that returns the short name of the subclass of C<Data::Domain> (i.e.
returns 'Int' for C<Data::Domain::Int>).

=head2 _expand_range

Internal utility method for converting a "range" parameter
into "min" and "max" parameters.

=head2 _call_lazy_domain

Internal utility method for dynamically converting
lazy domains (coderefs) into domains.


=head1 SEE ALSO

Doc and tutorials on complex Perl data structures:
L<perlref>, L<perldsc>, L<perllol>.

Other CPAN modules doing data validation :
L<Data::FormValidator|Data::FormValidator>,
L<CGI::FormBuilder|CGI::FormBuilder>,
L<HTML::Widget::Constraint|HTML::Widget::Constraint>,
L<Jifty::DBI|Jifty::DBI>,
L<Data::Constraint|Data::Constraint>,
L<Declare::Constraints::Simple|Declare::Constraints::Simple>,
L<Moose::Manual::Types>.
Among those, C<Declare::Constraints::Simple> is the closest to
C<Data::Domain>, because it is also designed to deal with
substructures; yet it has a different approach to combinations
of constraints and scope dependencies.

Some inspiration for C<Data::Domain> came from the wonderful
L<Parse::RecDescent|Parse::RecDescent> module, especially
the idea of passing a context where individual rules can grab
information about neighbour nodes.


=head1 TODO

  - generate javascript validation code
  - generate XML schema
  - normalization / conversions (-filter option)
  - msg callbacks (-filter_msg option)
  - default values within domains ? (good idea ?)

=head1 AUTHOR

Laurent Dami, E<lt>laurent.d...@etat.geneve.chE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright 2006, 2007, 2012 by Laurent Dami.

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself. 
