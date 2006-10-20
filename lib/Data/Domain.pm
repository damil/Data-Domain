#======================================================================
package Data::Domain; # documentation at end of file
#======================================================================
use strict;
use warnings;
use Exporter qw/import/;
use Carp;

our $VERSION = "0.01";

my @builtin_domains = qw/Whatever
                         Num Int Date Time String
                         Enum List Struct One_of/;

our %EXPORT_TAGS = (all => [@builtin_domains, 'node_from_path']);
Exporter::export_ok_tags('all');

# convenience functions : for each builtin domain, we export a closure
# that just calls new() on the corresponding subclass. For example,
# Num(@args) is just equivalent to Data::Domain::Num->new(@args).

foreach my $subclass (@builtin_domains) {
  no strict 'refs';
  my $full_class_name = "Data::Domain::" . $subclass;
  *{$subclass} = sub { $full_class_name->new(@_) };
}



my $builtin_msgs = {
 english => {
  Generic => { 
    INVALID => "invalid", 
    TOO_SMALL => "smaller than minimum %s",
    TOO_BIG   => "bigger than maximum %s", 
    EXCLUSION_SET => "belongs to exclusion set",
  },
  Whatever     => {
    MATCH_DEFINED => "data defined/undefined",
    MATCH_TRUE => "data true/false",
    MATCH_ISA =>  "is not a %s",
    MATCH_CAN =>  "does not have method %s",
   },
  Num     => {
    INVALID   => "invalid number",
  },
  Date => {
    INVALID   => "invalid date",
  },
  String => {
    TOO_SHORT        => "less than %d characters",
    TOO_LONG         => "more than %d characters",
    SHOULD_MATCH     => "should match %d",
    SHOULD_NOT_MATCH => "should not match %d",
  },
  Enum     => { NOT_IN_LIST => "not in enumeration list", },
  List     => {
    NOT_A_LIST => "is not an arrayref",
    TOO_SHORT  => "less than %d items",
    TOO_LONG   => "more than %d items",
  },
  Struct => { NOT_A_HASH => "is not a hashref", 
              FORBIDDEN_FIELD => "contains forbidden field: %s"
             },
 },

 "français" => {
  Generic => {
    INVALID => "incorrect", 
    TOO_SMALL => "plus petit que le minimum %s",
    TOO_BIG   => "plus grand que le maximum %s",
    EXCLUSION_SET => "fait partie des valeurs interdites",
   },
  Whatever     => {
    MATCH_DEFINED => "donnée définie/non définie",
    MATCH_TRUE => "donnée vraie/fausse",
    MATCH_ISA =>  "n'est pas un  %s",
    MATCH_CAN =>  "n'a pas la méthode %s",
   },
  Num => {
    INVALID   => "nombre incorrect",
   },
  Date => {
    INVALID   => "date incorrecte",
   },
  String => {
    TOO_SHORT => "moins de %d caractères",
    TOO_LONG  => "plus de %d caractères",
    SHOULD_MATCH     => "devrait être reconnu par la regex %d",
    SHOULD_NOT_MATCH => "ne devrait pas être reconnu par la regex %d",
  },
  Enum => {
    NOT_IN_LIST => "n'appartient pas à la liste énumérée",
   },
  List => {
    NOT_A_LIST => "n'est pas une arrayref",
    TOO_SHORT => "moins de %d éléments",
    TOO_LONG  => "plus de %d éléments",
   },
  Struct => {
    NOT_A_HASH    => "n'est pas une hashref",
    FORBIDDEN_FIELD => "contient le champ interdit: %s",
  },
 },
};

# inherit Int messages from Num messages
foreach my $key (keys %$builtin_msgs) {
  next if $key eq 'Generic';
  $builtin_msgs->{$key}{Int} = $builtin_msgs->{$key}{Num};
}


my $global_msgs = $builtin_msgs->{english};


sub messages {
  my ($class, $new_messages) = @_;
  croak "messages() is a class method" if ref $class;
  $global_msgs = (ref $new_messages) ? $new_messages 
                                     : $builtin_msgs->{$new_messages}
    or croak "no such builtin messages ($new_messages)";
}


sub msg {
  my ($self, $msg_id, @args) = @_;
  my $msgs     = $self->{-messages} || $global_msgs;
  my $subclass = $self->subclass;
  my $name     = $self->{-name} || $subclass;

  for (ref $msgs) {
    /CODE/ and return $msgs->($msg_id, @args);
    /HASH/ and do {
      my $msg = $msgs->{$subclass}{$msg_id} 
             || $msgs->{Generic}{$msg_id} 
             || $msgs->{$msg_id} 
          or croak "no error string for message $msg_id";
      return "$name: " . sprintf($msg, @args);
    };
    /^$/ and $msgs and return "$name: $msgs"; # just plain string

    #otherwise
    croak "can't generate error message ($msgs)";
  }
}


sub subclass {
  my ($self) = @_;
  my $class = ref($self) || $self;
  (my $subclass = $class) =~ s/^Data::Domain:://;
  return $subclass;
}


sub inspect {
  my ($self, $data, $context) = @_;
  return if $self->{-optional} and not defined($data);
  return $self->_inspect($data, $context); # otherwise
}


# UTILITY FUNCTIONS (NOT METHODS) 

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
    if UNIVERSAL::isa($root, 'HASH');
  return node_from_path($root->[$path0], @path) 
    if UNIVERSAL::isa($root, 'ARRAY');

  # otherwise
  croak "node_from_path: incorrect root/path";
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
  my @options = qw/-defined -true -isa -can/;
  my $self    = Data::Domain::_parse_args( \@_, \@options );
  croak "both -defined and -optional: meaningless!"
    if $self->{-defined } and $self->{-optional};
  bless $self, $class;
}

sub _inspect {
  my ($self, $data) = @_;

  if (defined $self->{-defined}) {
    return $self->msg(MATCH_DEFINED => $self->{-defined})
      if defined($data) xor $self->{-defined};
  }
  if (defined $self->{-true}) {
    return $self->msg(MATCH_TRUE => $self->{-true})
      if $data xor $self->{-true};
  }
  if (defined $self->{-isa}) {
    UNIVERSAL::isa($data, $self->{-isa})
      or return $self->msg(MATCH_ISA => $self->{-isa});
  }
  if (defined $self->{-can}) {
    my $methods = ref($self->{-can}) ? $self->{-can} : [$self->{-can}];
    foreach my $method (@$methods) {
      UNIVERSAL::can($data, $method)
        or return $self->msg(MATCH_CAN => $method);
    }
  }
  return;    #otherwise
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
  my @options = qw/-min -max -not_in/;
  my $self = Data::Domain::_parse_args(\@_, \@options);
  if (defined($self->{-min}) and defined($self->{-max})) {
    $self->{-min} <= $self->{-max}
      or croak "Num domain: incoherent min/max values";
  }
  if ($self->{-not_in}) {
    eval {my $vals = $self->{-not_in};
          @$vals > 0 and not grep {!looks_like_number($_)} @$vals}
      or croak "-not_in : needs an arrayref of numbers";
  }
  bless $self, $class;
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
our @ISA = 'Data::Domain';

sub new {
  my $class = shift;
  my @options = qw/-regex -antiregex
                   -min_length -max_length 
                   -min -max -not_in/;
  my $self = Data::Domain::_parse_args(\@_, \@options, -regex => 'scalar');
  bless $self, $class;
}

sub _inspect {
  my ($self, $data) = @_;

  defined($data)
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

# TODO: extend API to choose between EU/US dates

use autouse 'Date::Calc' => qw/Decode_Date_EU Decode_Date_US Date_to_Text
                               Delta_Days  Add_Delta_Days Today/;

my $parser = \&Decode_Date_EU;

sub parser {
  my ($class, $new_parser) = @_;
  if ($new_parser) {
    if (ref $new_parser) {
      $parser = $new_parser;
    }
    else {
      $parser = ($new_parser eq 'US') ? \&Decode_Date_US
               : ($new_parser eq 'EU') ? \&Decode_Date_EU
               : croak "unknown date parser : $new_parser";
    }
  }
  return $parser;
}

sub _print_date {
  my $date = shift;
  return ref($date) ? Date_to_Text(@$date) : $date;
}


my $dynamic_date = qr/^(today|yesterday|tomorrow)$/;

sub _date_cmp {
  my ($d1, $d2) = @_;
  if (!ref($d2)) {
    $d2 = {
      today     => [Today], 
      yesterday => [Add_Delta_Days(Today, -1)],
      tomorrow  => [Add_Delta_Days(Today, +1)]
     }->{$d2} or croak "unexpected date : $d2";
  }
  return -Delta_Days(@$d1, @$d2);
}

sub new {
  my $class   = shift;
  my @options = qw/-min -max -not_in/;
  my $self    = Data::Domain::_parse_args(\@_, \@options);

  if ($self->{-min} and $self->{-min} !~ $dynamic_date) {
    my @min_date = $parser->($self->{-min})
      or croak "invalid date: $self->{-min}";
    $self->{-min} = \@min_date;
  }

  if ($self->{-max} and $self->{-max} !~ $dynamic_date) {
    my @max_date = $parser->($self->{-max})
      or croak "invalid date: $self->{-max}";
    $self->{-max} = \@max_date;
  }

  if ($self->{-not_in}) {
    my @excl_dates;
    eval {
      foreach my $date (@{$self->{-not_in}})
      {
        if ($date =~ $dynamic_date) {
          push @excl_dates, $date;
        }
        else {
          my @parsed_date = $parser->($date) or die "wrong date";
          push @excl_dates, \@parsed_date;
        }
      }
      @excl_dates > 0;
    }
      or croak "-not_in : needs an arrayref of dates";
    $self->{-not_in} = \@excl_dates;
  }

  bless $self, $class;
}


sub _inspect {
  my ($self, $data) = @_;

  my @date = $parser->($data) 
    or return $self->msg(INVALID => $data);

  if (defined $self->{-min}) {
    _date_cmp(\@date, $self->{-min}) < 0
      and return $self->msg(TOO_SMALL => _print_date($self->{-min}));
  }

  if (defined $self->{-max}) {
    _date_cmp(\@date, $self->{-max}) > 0
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

sub _time_cmp {
  my ($t1, $t2) = @_;
  $t2 = [(localtime)[2, 1, 0]] if $t2 eq 'now';

  return  $t1->[0]       <=>  $t2->[0]        # hours
      || ($t1->[1] || 0) <=> ($t2->[1] || 0)  # minutes
      || ($t1->[2] || 0) <=> ($t2->[2] || 0);  # seconds
}

sub new {
  my $class = shift;
  my @options = qw/-min -max/;
  my $self = Data::Domain::_parse_args(\@_, \@options);

  if ($self->{-min} and $self->{-min} ne 'now') {
    my @min_time = ($self->{-min} =~ $time_regex);
    @min_time && _valid_time(@min_time)
      or croak "invalid time: $self->{-min}";
    $self->{-min} = \@min_time;
  }

  if ($self->{-max} and $self->{-max} ne 'now') {
    my @max_time = ($self->{-max} =~ $time_regex);
    @max_time && _valid_time(@max_time)
      or croak "invalid time: $self->{-max}";
    $self->{-max} = \@max_time;
  }

  bless $self, $class;
}


sub _inspect {
  my ($self, $data) = @_;

  my @t = ($data =~ $time_regex);
  @t and _valid_time(@t)
    or return $self->msg(INVALID => $data);

  if (defined $self->{-min}) {
    _time_cmp(\@t, $self->{-min}) < 0
      and return $self->msg(TOO_SMALL => $data);
  }

  if (defined $self->{-max}) {
    _time_cmp(\@t, $self->{-max}) > 0
      and return $self->msg(TOO_BIG => $data);
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
  eval {@{$self->{-values}}} or croak "Enum is empty";
  bless $self, $class;
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
  my @options = qw/-items -min_size -max_size -any -all/;
  my $self = Data::Domain::_parse_args(\@_, \@options, -items => 'arrayref');
  croak "can't have both -all and -any" 
    if $self->{-all} and $self->{-any};

  if ($self->{-items}) {
    ref($self->{-items}) eq 'ARRAY' 
      or croak "invalid -items for Data::Domain::List";
    croak "-min_size does not match -items"
      if $self->{-min_size} and $self->{-min_size} < @{$self->{-items}};

#    $self->{-min_size} ||= @{$self->{-items}};
  }

  bless $self, $class;
}


sub _inspect {
  my ($self, $data, $context) = @_;

  UNIVERSAL::isa($data, 'ARRAY')
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
  my ($has_invalid, $has_good_item);
  my $items = $self->{-items} || [];
  my $n_checks = max(scalar(@$data), scalar(@$items));

  for (my $i = 0; $i < $n_checks; $i++) {
    my $subdomain = $items->[$i] || $self->{-all} || $self->{-any}
      or next;
    local $context->{path} = [@{$context->{path}}, $i];

    # if lazy domain, call it
    $subdomain = $subdomain->($context) 
      if UNIVERSAL::isa($subdomain, 'CODE');

    $msgs[$i] = $subdomain->inspect($data->[$i], $context);
    $has_invalid   ||=   $msgs[$i];
    $has_good_item ||= ! $msgs[$i];
  }

  return \@msgs if ($has_invalid    and $self->{-items} || $self->{-all}) 
                or (!$has_good_item and $self->{-any});

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

  my $fields = $self->{-fields} || [];
  for (ref $fields) {

    # transform arrayref into hashref plus an ordered list of keys
    /ARRAY/ and do { 
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
    /HASH/ and do {
      $self->{-fields_list} = [keys %$fields];
      last;
    };

    # otherwise
    croak "invalid data for -fields option";
  }

  croak "invalid data for -exclude option" 
    if $self->{-exclude} and ref($self->{-exclude}) !~ /^(ARRAY|Regexp|)$/;

  bless $self, $class;
}


sub _inspect {
  my ($self, $data, $context) = @_;

  # check that $data is a hashref
  UNIVERSAL::isa($data, 'HASH')
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
    my $subdomain = $self->{-fields}{$field};
    local $context->{path} = [@{$context->{path}}, $field];

    # if lazy domain, call it
    $subdomain = $subdomain->($context)
      if UNIVERSAL::isa($subdomain, 'CODE');

    my $msg = $subdomain->inspect($data->{$field}, $context);
    $msgs{$field} = $msg if $msg;
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
  $self->{-options} and ref($self->{-options}) eq 'ARRAY'
    or croak "One_of: invalid options";
  bless $self, $class;
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
1;


__END__

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
    aMixedList => List(Integer, String, Int(-min => 0), Date),
    aStruct    => Struct(foo => String, bar => Int(-optional => 1))
  );

  my $messages = $domain->inspect($some_data);
  my_display_error($messages) if $messages;

=head1 DESCRIPTION

A data domain is a description of a set of values, either
scalar, or, more interestingly, structured values (arrays or hashes).
The description can include many constraints, like minimal or maximal
values, regular expressions, required fields, forbidden fields, and
also contextual dependencies. From that description, one can then
check if a given value belongs to the domain. In case of mismatch,
a structured set of error messages is returned.

The motivation for writing this package was to be able to express in a
compact way some possibly complex constraints about structured
data. The data is a Perl tree (nested hashrefs or arrayrefs) that may
come from XML, L<JSON|JSON>, from a database through
L<DBIx::DataModel|DBIx::DataModel>, or from postprocessing an HTML form
through L<CGI::Expand|CGI::Expand>. C<Data::Domain> is a kind of tree
parser on that structure, with some facilities for dealing
with dependencies within the structure through lazy 
evaluation of domains. Other packages doing data validation are
briefly listed in the L</"SEE ALSO"> section. 

B<DISCLAIMER : this code is still in design exploration phase; 
  some parts of the API may change in future versions>.

=head1 FUNCTIONS

=head2 Shortcuts for domain constructors

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
or C<use Data::Domain qw/Struct Int Date .../>),
then you can write more conveniently :

  my $domain = Struct(
    anInt      => Int(-min => 3, -max => 18),
    aDate      => Date(-max => 'today'),
    ...
  );

Function names like C<Int> or C<String> are convenient, but also quite
common and therefore may cause name clashes with other modules.  In
such cases, don't import the function names, and explicitly call the
<new> method on domain constructors -- or write your own wrappers
around them.

=head2 node_from_path

  my $node = node_from_path($root, @path);

Convenience function to find a given node in a data tree, starting
from the root and following a I<path> (a sequence of hash keys or
array indices). Returns C<undef> if no such path exists in the tree.
Mainly useful for contextual constraints in lazy constructors
(see below).


=head1 METHODS

=head2 new

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

if true, an <undef> value will be accepted, without generating an
error message

=item C<-default>

defines a default value for the domain, that can then be retrieved
by client code, for example for pre-filling a form

=item C<-name>

defines a name for the domain, that will be printed in error
messages instead of the subclass name. 

=item C<-messages>

defines how error messages will be generated


=back

Option names always start with a dash. If no option name is given,
parameters to the C<new> method are passed to the I<default option>,
which differs according to the constructor subclass. For example
the default option in  C<List> is C<-items>, so 

   my $domain = List(Int, String, Int);

is equivalent to

   my $domain = List(-items => [Int, String, Int]);


=head2 inspect

  my $messages = $domain->inspect($some_data);

Inspects the supplied data, and returns an error message
(or a structured collection of messages) if anything is wrong.
If the data successfully passed all domain tests, then nothing
is returned.

For scalar domains (C<Num>, C<String>, etc.), the error message
is just a string. For structured domains (C<List>, C<Struct>),
the return value is a corresponding arrayref or hashref, like
for example

  {anInt => "smaller than mimimum 3",
   aDate => "not a valid date",
   aList => ["message for item 0", undef, undef, "message for item 3"]}

The client code can then exploit this structure to dispatch 
error messages to appropriate locations (typically these
will be the form fields that gathered the data).


=head2 msg

Internal utility function for generating an error message.

=head2 subclass

Returns the short name of the subclass of C<Data::Domain> (i.e.
returns 'Int' for C<Data::Domain::Int>).


=head2 messages

Global setting to choose how error messages are generated. The argument

  Data::Domain->messages('english');  # the default
  Data::Domain->messages('français');
  Data::Domain->messages($my_messages); 
  Data::Domain->messages(sub {my ($msg_id, @args) = @_;
                              return "you just got it wrong"});

Global setting to choose how error messages are generated. The argument
is either the name of a builtin language (right now, only english and french),
or a hashref with your own messages, or a reference to your own 
message handling function.

If supplying your own messages, you should pass a two-level hashref:
first-level entries in the hash correspond to C<Data::Domain> subclasses
(i.e C<< Num => {...} >>, C<< String => {...} >>); for each of 
those, the second-level entries should correspond
to message identifiers as specified in the doc for each subclass
(for example C<TOO_SHORT>, C<NOT_A_HASH>, etc.).
Values should be strings suitable to be fed to L<sprintf>.



=head1 BUILTIN DOMAIN CONSTRUCTORS

=head2 Whatever

  my $domain = Struct(
    just_anything => Whatever,
    is_defined    => Whatever(-defined => 1),
    is_undef      => Whatever(-defined => 0),
    is_true       => Whatever(-true => 1),
    is_false      => Whatever(-true => 0),
    is_object     => Whatever(-isa => 'My::Funny::Object'),
    has_methods   => Whatever(-can => [qw/jump swim dance sing/]),
  );

Encapsulates just any kind of Perl value. 

=head3 Options

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

=back

=head3 Error messages

In case of failure, the domain returns one of the following scalar
messages : C<MATCH_DEFINED>, C<MATCH_TRUE>, C<MATCH_ISA>, C<MATCH_CAN>.


=head2 Num

  my $domain = Num(-min => -3.33, -max => 999, -not_in => [2, 3, 5, 7, 11]);

Domain for numbers (including floats).

=head3 Options

=over

=item -min

The data must be greater or equal to the supplied value.

=item -max

The data must be smaller or equal to the supplied value.

=item -not_in

The data must be different from all values in the exclusion set,
supplied as an arrayref.

=back


=head3 Error messages

In case of failure, the domain returns one of the following scalar
messages : C<INVALID>, C<TOO_SMALL>, C<TOO_BIG>, C<EXCLUSION_SET>.



=head2 Int

  my $domain = Int(-min => 0, -max => 999, -not_in => [2, 3, 5, 7, 11]);

Domain for integers. Accepts the same options as C<Num> and returns the 
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



=head3 Options

In the options below, the special keywords C<today>, 
C<yesterday> or C<tomorrow> may be used instead of a date
constant, and will be replaced by the appropriate date
when performing comparisons.

=over

=item -min

The data must be greater or equal to the supplied value.

=item -max

The data must be smaller or equal to the supplied value.

=item -not_in

The data must be different from all values in the exclusion set,
supplied as an arrayref.

=back


=head3 Error messages

In case of failure, the domain returns one of the following scalar
messages : C<INVALID>, C<TOO_SMALL>, C<TOO_BIG>, C<EXCLUSION_SET>.



=head2 Time

  my $domain = Time(-min => '08:00', -max => 'now');

Domain for times in format C<hh:mm:ss> (minutes and seconds are optional).


=head3 Options

In the options below, the special keyword C<now> may be used instead of a 
time, and will be replaced by the current local time
when performing comparisons.

=over

=item -min

The data must be greater or equal to the supplied value.

=item -max

The data must be smaller or equal to the supplied value.

=back


=head3 Error messages

In case of failure, the domain returns one of the following scalar
messages : C<INVALID>, C<TOO_SMALL>, C<TOO_BIG>.


=head2 String

  my $domain = String(qr/^[A-Za-z0-9_\s]+$/);

  my $domain = String(-regex     => qr/^[A-Za-z0-9_\s]+$/,
                      -antiregex => qr/$RE{profanity}/,    # see Regexp::Common
                      -min       => 'AA',
                      -max       => 'zz',
                      -not_in    => [qw/foo bar/]);

Domain for strings.

=head3 Options


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

=item -not_in

The data must be different from all values in the exclusion set,
supplied as an arrayref.

=back


=head3 Error messages

In case of failure, the domain returns one of the following scalar
messages : C<TOO_SHORT>, C<TOO_LONG>, C<TOO_SMALL>, C<TOO_BIG>,
C<EXCLUSION_SET>, C<SHOULD_MATCH>, C<SHOULD_NOT_MATCH>.



=head2 Enum

  my $domain = Enum(qw/foo bar buz/);

Domain for a finite set of scalar values.

=head3 Options

=over

=item -values

Ref to an array of values admitted in the domain. 
This would be called as C<< Enum(-values => [qw/foo bar buz/]) >>,
but since this it is the default option, it can be
simply written as C<< Enum(qw/foo bar buz/) >>.

=back

=head3 Error messages

In case of failure, the domain returns the following scalar
message : C<NOT_IN_LIST>.


=head2 List

  my $domain = List(String, Int, String, Num);

  my $domain = List(-items => [String, Int, String, Num]); # same as above

  my $domain = List(-all      => Int(qr/^[A-Z]+$/),
                    -min_size => 3,
                    -max_size => 10);


Domain for lists of values (stored as Perl arrayrefs).

=head3 Options

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

=item -all

All remaining entries in the array, after the first <n> entries
as specified by the C<-items> option (if any), must satisfy that
domain specification.

=item -any

At least one remaining entry in the array, after the first <n> entries
as specified by the C<-items> option (if any), must satisfy that
domain specification.

Option C<-any> is incompatible with option C<-all>.

=back

=head3 Error messages

The domain will first check if the supplied array is of appropriate
shape; in case of of failure, it will return of the following scalar
messages :  C<NOT_A_LIST>, c<TOO_SHORT>, C<TOO_LONG>.

Then it will check all items in the supplied array according to 
the C<-items>, C<-all> or C<-any> specifications, and return an
arrayref of messages, where message positions correspond to the
positions of offending data items.


=head2 Struct

  my $domain = Struct(foo => Int, bar => String);

  my $domain = Struct(-fields  => [foo => Int, bar => String],
                      -exclude => '*');

Domain for associative structures (stored as Perl hashrefs).

=head3 Options

=over

=item -fields

Supplies a list of keys with their associated domains. The list might
be given either as a hashref or as an arrayref (in which case the the
order of individual field checks will follow the order in the array).
The ordering may make a difference in case of context dependencies (see 
L<"LAZY CONSTRUCTORS"|/"LAZY CONSTRUCTORS (CONTEXT DEPENDENCIES)"> below ).


=item -exclude

Specifies which keys are not allowed in the structure. The exclusion 
may be specified as an arrayref of key names, as a compiled regular
expression, or as the string constant 'C<*>' or 'C<all>' (meaning
that no key will be allowed except those explicitly listed in the 
C<-fields> option.

=back



=head3 Error messages

The domain will first check if the supplied hash is of appropriate
shape; in case of of failure, it will return of the following scalar
messages :  C<NOT_A_HASH>, c<FORBIDDEN_FIELD>.

Then it will check all entries in the supplied hash according to 
the C<-fields> specification, and return a
hashref of messages, where keys correspond to the
keys of offending data items.


=head2 One_of

  my $domain = One_of($domain1, $domain2, ...);

Union of domains : successively checks the member domains,
until one of them succeeds. 

=head3 Options

=over

=item -options

List of domains to be checked. This is the default option, so 
the keyword may be omitted.

=back

=head3 Error messages

If all member domains failed to accept the data, an arrayref
or error messages is returned, where the order of messages
corresponds to the order of the checked domains.




=head1 LAZY CONSTRUCTORS (CONTEXT DEPENDENCIES)

=head2 Principle

If an element of a structured domain (C<List> or C<Struct> depends on 
another element), then we need to I<lazily> construct the domain.
Consider for example a struct in which the value of field C<date_end> 
must be greater than C<date_begin> : 
the subdomain for C<date_end> can only be constructed 
when the argument to <-min> is known, namely when
the domain inspects an actual data structure.

Lazy domain construction is achieved by supplying a function reference
instead of a domain object. That function will be called with some
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
to other ancestor or sibling data node within the tree, with 
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
mechanisms are not enough for your needs. For example, many
domains could be expressed as a C<String> with a regular expression:

  my $Email_dom   = String(qr/^[-.\w]+\@[\w.]+$/); 
  my $Phone_dom   = String(qr/^\+?[0-9() ]+$/); 
  my $Contact_dom = Struct(name   => String,
                           phone  => $Phone_dom,
                           mobile => $Phone_dom,
                           emails => List(-all => $Email_dom));


=head1 SEE ALSO

Doc and tutorials on complex Perl data structures:
L<perlref>, L<perldsc>, L<perllol>.

Other CPAN modules doing data validation :
L<Data::FormValidator|Data::FormValidator>,
L<CGI::FormBuilder|CGI::FormBuilder>,
L<HTML::Widget::Constraint|HTML::Widget::Constraint>,
L<Jifty::DBI|Jifty::DBI>,
L<Data::Constraint|Data::Constraint>,
L<Declare::Constraints::Simple|Declare::Constraints::Simple>.
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
  - normalization / conversions (-filter option)
  - msg callbacks (-filter_msg option)
  - default values within domains ? (good idea ?)

=head1 AUTHOR

Laurent Dami, E<lt>laurent.dami AT etat  geneve  chE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright 2006 by Laurent Dami.

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself. 
