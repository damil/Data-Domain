=begin TODO

  - generate javascript
  - API for choosing msg language
  - API for domain-specific messages
  - normalization ?? conversions ??
  - order of fields in Struct hashes is important

=end TODO

=cut


#======================================================================
package Data::Domain;
#======================================================================
use strict;
use warnings;
use Exporter qw/import/;
use Carp;

our $VERSION = "0.01";

my @builtin_domains = qw/Num Int Date Time String
                         Whatever
                         Enum List Struct/;

# convenience functions : for each builtin domain, we export a closure
# that just calls new() on the corresponding subclass. For example,
# Num(@args) is just equivalent to Data::Domain::Num->new(@args).

our %EXPORT_TAGS = (all  => \@builtin_domains);
Exporter::export_ok_tags('all'); 
foreach my $subclass (@builtin_domains) {
  no strict 'refs';
  my $full_class_name = "Data::Domain::" . $subclass;
  *{$subclass} = sub {$full_class_name->new(@_)};
}


my $EN_msgs = {
  Generic => {
    UNDEFINED => "empty",
   },
  Num => {
    INVALID   => "invalid number",
    TOO_SMALL => "smaller than minimum %s",
    TOO_BIG   => "bigger than maximum %s",
   },
  String => {
    TOO_SHORT => "less than %d characters",
    TOO_LONG  => "more than %d characters",
    MISMATCH  => "does not match regex %s",
  },
  Date => {
    INVALID   => "invalid date",
    TOO_SMALL => "smaller than minimum %s",
    TOO_BIG   => "bigger than maximum %s",
   },
  Enum => {
    NOT_IN_LIST => "not in enumeration list",
   },
  CodeList => {
    NOT_IN_LIST => "not in code list",
   },
  List => {
    NOT_A_LIST => "is not an arrayref",
    TOO_SHORT => "less than %d items",
    TOO_LONG  => "more than %d items",
   },
  Struct => {
    NOT_A_HASH    => "is not a hashref",
  },
};


# NOT_DEFINED
# DEFINED
# WRONG_TRUTH
# MATCH
# UNMATCH


my $FR_msgs = {
  Generic => {
    UNDEFINED => "vide",
   },
  Num => {
    INVALID   => "nombre incorrect",
    TOO_SMALL => "plus petit que le minimum %s",
    TOO_BIG   => "plus grand que le maximum %s",
   },
  String => {
    TOO_SHORT => "moins de %d caractères",
    TOO_LONG  => "plus de %d caractères",
    MISMATCH  => "refusé par la regex %s",
  },
  Date => {
    INVALID   => "date incorrecte",
    TOO_SMALL => "plus petite que le minimum %s",
    TOO_BIG   => "plus grande que le maximum %s",
   },
  Enum => {
    NOT_IN_LIST => "n'appartient pas à la liste énumérée",
   },
  CodeList => {
    NOT_IN_LIST => "n'appartient pas à la liste de codes",
   },
  List => {
    NOT_A_LIST => "n'est pas une arrayref",
    TOO_SHORT => "moins de %d éléments",
    TOO_LONG  => "plus de %d éléments",
   },
  Struct => {
    NOT_A_HASH    => "n'est pas une hashref",
  },
};


my $msgs = $EN_msgs;


sub msg {
  my ($self, $msg_id, @args) = @_;
  my $class = ref($self) || $self;

  (my $subclass = $class) =~ s/^Data::Domain:://;
  my $msg = $msgs->{$subclass}{$msg_id} 
         || $msgs->{Generic}{$msg_id}
    or croak "invalid message id: $msg_id";

  return sprintf($msg, @args);
}



sub inspect {
  my ($self, $data) = @_;
  return if $self->{-optional} and not defined($data);
  return $self->_inspect($data); # otherwise
}


# UTILITY FUNCTIONS (NOT METHODS) 

# valid options for all subclasses
my @common_options = qw/-optional -default/; 

sub _parse_args {
  my ($args_ref, $options_ref) = @_;

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
    my $default_option = $options_ref->[0];
    croak "can't have default args if $default_option is set"
      if exists $parsed{$default_option};
    $parsed{$default_option} = $args_ref;
  }
  return \%parsed;
}


#======================================================================
package Data::Domain::Whatever;
#======================================================================
use strict;
use warnings;
our @ISA = 'Data::Domain';

sub new {
  my $class = shift;
  my @options = qw/-defined -true -isa -can/;
  my $self = Data::Domain::_parse_args(\@_, \@options);
  croak "both -defined and -optional: meaningless!"
    if $self->{-defined} and $self->{-optional};
  bless $self, $class;
}

sub _inspect {
  my ($self, $data) = @_;

  if (defined $self->{-defined}) {
    return $self->msg(DEFINED => $self->{-defined})
      if defined($data) xor $self->{-defined};
  }
  if (defined $self->{-true}) {
    return $self->msg(TRUE => $self->{-true})
      if $data xor $self->{-true};
  }
  if (defined $self->{-isa}) {
    UNIVERSAL::isa($data, $self->{-isa})
      or return $self->msg(ISA => $self->{-isa});
  }
  if (defined $self->{-can}) {
    my $methods = ref($self->{-can}) ? $self->{-can} : [$self->{-can}];
    foreach my $method (@$methods) {
      UNIVERSAL::can($data, $method)
          or return $self->msg(CAN => $method);
    }
  }
  return; #otherwise
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

  if ($self->{-min}) {
    $data >= $self->{-min} 
      or return $self->msg(TOO_SMALL => $self->{-min});
  }
  if ($self->{-max}) {
    $data <= $self->{-max} 
      or return $self->msg(TOO_BIG => $self->{-max});
  }
  if ($self->{-not_in}) {
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

  $data =~ /^-?\d+$/
    or return $self->msg(INVALID => $data);
  return $self->SUPER::_inspect($data);
}


#======================================================================
package Data::Domain::String;
#======================================================================
use strict;
use warnings;
our @ISA = 'Data::Domain';

sub new {
  my $class = shift;
  my @options = qw/-regex -antiregex
                   -min_length -max_length 
                   -min -max -not_in/;
  my $self = Data::Domain::_parse_args(\@_, \@options);
  bless $self, $class;
}

sub _inspect {
  my ($self, $data) = @_;

  if ($self->{-min_length}) {
    length($data) >= $self->{-min_length} 
      or return $self->msg(TOO_SHORT => $self->{-min_length});
  }
  if ($self->{-max_length}) {
    length($data) <= $self->{-max_length} 
      or return $self->msg(TOO_LONG => $self->{-max_length});
  }
  if ($self->{-regex}) {
    $data =~ $self->{-regex}
      or return $self->msg(MISMATCH => $self->{-regex});
  }
  if ($self->{-antiregex}) {
    $data !~ $self->{-antiregex}
      or return $self->msg(MATCH => $self->{-antiregex});
  }
  if ($self->{-min}) {
    $data ge $self->{-min} 
      or return $self->msg(TOO_SMALL => $self->{-min});
  }
  if ($self->{-max}) {
    $data le $self->{-max} 
      or return $self->msg(TOO_BIG => $self->{-max});
  }
  if ($self->{-not_in}) {
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

my $decoder = \&Decode_Date_EU;

sub decoder {
  my ($class, $new_decoder) = @_;
  if ($new_decoder) {
    if (ref $new_decoder) {
      $decoder = $new_decoder;
    }
    else {
      $decoder = ($new_decoder eq 'US') ? \&Decode_Date_US
               : ($new_decoder eq 'EU') ? \&Decode_Date_EU
               : croak "unknown date decoder : $new_decoder";
    }
  }
  return $decoder;
}


my $dynamic_date = qr/^(today|yesterday|tomorrow)$/;

sub _date_cmp {
  my ($d1, $d2) = @_;
  if (!ref($d2)) {
    for ($d2) {
      /today/     and $d2 = [Today], last;
      /yesterday/ and $d2 = [Add_Delta_Days(Today, -1)], last;
      /tomorrow/  and $d2 = [Add_Delta_Days(Today, +1)], last;

      # otherwise
      croak "unexpected date : $d2";
    }
  }
  return Delta_Days(@$d1, @$d2);
}

sub new {
  my $class = shift;
  my @options = qw/-min -max -not_in/;
  my $self = Data::Domain::_parse_args(\@_, \@options);

  if ($self->{-min} and $self->{-min} !~ $dynamic_date) {
    my @min_date = $decoder->($self->{-min})
      or croak "invalid date: $self->{-min}";
    $self->{-min} = \@min_date;
  }
  if ($self->{-max} and $self->{-max} !~ $dynamic_date) {
    my @max_date = $decoder->($self->{-max})
      or croak "invalid date: $self->{-max}";
    $self->{-max} = \@max_date;
  }
  if ($self->{-not_in}) {
    my @excl_dates;
    eval {
      foreach my $date (@{$self->{-not_in}}) {
        if ($date =~ $dynamic_date) {
          push @excl_dates, $date;
        }
        else {
          my @parsed_date = $decoder->($date) or die "wrong date";
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

  my @date = $decoder->($data) 
    or return $self->msg(INVALID => $data);

  if ($self->{-min}) {
    _date_cmp(\@date, $self->{-min}) <= 0
      or return $self->msg(TOO_SMALL => Date_to_Text(@min));
  }

  if ($self->{-max}) {
    _date_cmp(\@date, $self->{-max}) >= 0
      or return $self->msg(TOO_BIG => Date_to_Text(@max));
  }

  if ($self->{-not_in}) {
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

my $time_regex = qr/^(\d\d?)(?::\d\d?)(?::\d\d?)$/;

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
      || ($t1->[2] || 0) <=> ($t2->[2] || 0)  # seconds
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

  if ($self->{-min}) {
    _time_cmp(\@t, $self->{-min}) <= 0
      or return $self->msg(TOO_SMALL => $data);
  }

  if ($self->{-max}) {
    _time_cmp(\@t, $self->{-max}) <= 0
      or return $self->msg(TOO_BIG => $data);
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
  my $self = Data::Domain::_parse_args(\@_, \@options);
  @{$self->{-values}} or croak "Enum is empty";
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
our @ISA = 'Data::Domain';

sub new {
  my $class = shift;
  my @options = qw/-items -min_size -max_size -any -all/;
  my $self = Data::Domain::_parse_args(\@_, \@options);
  croak "can't have both -all and -any" 
    if $self->{-all} and $self->{-any};
  bless $self, $class;
}


sub _inspect {
  my ($self, $data, $context) = @_;

  UNIVERSAL::isa($data, 'ARRAY')
    or return $self->msg(NOT_A_LIST => $data);

  if ($self->{-min_size} && @$data < $self->{-min_size}) {
    return $self->msg(TOO_SHORT => $self->{-min_size});
  }

  if ($self->{-max_size} && @$data > $self->{-max_size}) {
    return $self->msg(TOO_LONG => $self->{-max_size});
  }


  return unless $self->{-items} || $self->{-all} || $self->{-any};

  $context ||= {root => $data,
                glob => {},
                path => []};

  my @msgs;
  my ($has_invalid, $has_good_item);
  my $items = $self->{-items} || [];

  for (my $i = 0; $i < @$data; $i++) {
    my $subdomain = $items->[$i] || $self->{-all} || $self->{-any};
    if (UNIVERSAL::isa($subdomain, 'CODE')) {
      local $context->{path} = [@{$context->{path}}, $i];
      $subdomain = $subdomain->($context);
    }
    $msgs[$i] = $subdomain->inspect($data->[$i]);
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
our @ISA = 'Data::Domain';

# TODO: somehow keep the order of field names

sub new {
  my $class = shift;
  my @options = qw/-fields -exclude/;
  my $self = Data::Domain::_parse_args(\@_, \@options);
  $self->{-fields} = {@{$self->{-fields}}}; # convert arrayref to hashref
  bless $self, $class;
}


sub _inspect {
  my ($self, $data, $context) = @_;

  # check that $data is a hashref
  UNIVERSAL::isa($data, 'HASH')
    or return $self->msg(NOT_A_HASH => $data);

  # check if there are any forbidden fields
  if ($self->{-exclude}) {
    foreach my $field (keys %$data) {
      next if $self->{-fields}{$field};

      my $ref = ref $self->{-exclude};
      return $self->msg(FORBIDDEN_FIELD => $field)
        if $ref eq 'ARRAY'  and grep {$field eq $_} @{$self->{-exclude}};
        or $ref eq 'Regexp' and $field =~ $self->{-exclude};
        or $ref eq ''       and $self->{-exclude} =~ /\*|all/;
    }
  }

  # then check if all other fields match the constraints of the domain
  my %msgs;
  my $has_invalid;
  $context ||= {root => $data,
                glob => {},
                path => []};
  local $context->{glob} = {%{$context->{glob}}, %$data};
  while (my ($field, $domain) = each %{$self->{-fields}}) {

    if (UNIVERSAL::isa($domain, 'CODE')) {
      local $context->{path} = [@{$context->{path}}, $field];
      $domain = $domain->($context);
    }

    my $msg = $domain->inspect($data->{$field});
    $msgs{$field} = $msg if $msg;
    $has_invalid ||=  $msg;
  }

  return $has_invalid ? \%msgs : undef;
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
                       Date(-min => $context->{glob}{aDate})},
    aString    => String(-min_length => 2, -optional => 1),
    anEnum     => Enum(qw/foo bar buz/),
    aCodeList  => CodeList(foo => "This is the foo value",
                           bar => "This is the bar value"),
    anIntList  => List(-min_size => 1, -all => Int),
    aMixedList => List(Integer, String, Int(-min => 0), Date),
    aStruct    => Struct(foo => String, bar => Int(-optional => 1))
  );

  my $messages = $domain->inspect($some_data);
  display_error($messages) if $messages;

=head1 DESCRIPTION

A data domain is a description of a set of values, either
scalar, or, more interestingly, structured values (arrays or hashes).
The description can include many constraints, like minimal or maximal
values, regular expressions, required fields, forbidden fields, and
also contextual dependencies. From that description, one can :

=over

=item *

check if a given value belongs to the domain. In case of mismatch,
a structured set of error messages is returned.

=item *

[[ build a default value ; NOT IMPLEMENTED; GOOD IDEA ?? ]]

=item * 

generate Javascript validation code

=back


=head1 FUNCTIONS

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

=head1 METHODS

=head2 new

Creates a new domain object, from one of the domain constructors
listed below (C<Num>, C<Int>, C<Date>, etc.). 
The C<Data::Domain> class itself has no
C<new> method, because it is an abstract class.

Arguments to the C<new> method specify various constraints for the
domain (minimal/maximal values, regular expressions, etc.); most often
they are specific to a given domain constructor, so see the details
below. However, there are also some generic arguments :

=over

=item C<-optional>

if true, an <undef> value will be accepted, without generating an
error message

=item C<-default>

defines a default value for the domain, that can then be retrieved
by client code, for example for pre-filling a form

=back


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

=head2 set_language

[[API for choosing msgs language]]




=head1 BUILTIN DOMAIN CONSTRUCTORS

=head2 Whatever

  my $domain = Struct(
    is_defined  => Whatever(-defined => 1),
    is_true     => Whatever(-true => 1),
    is_false    => Whatever(-true => 0),
    is_object   => Whatever(-isa => 'My::Funny::Object'),
    has_methods => Whatever(-can => [qw/jump swim dance sing/]),
  );

Encapsulates just any kind of Perl value. 

=head2 Options

=over

=item -defined

=item -true

=item -isa

=item -can

=back




=head2 Num

=head2 Options

=over

=item -min

=item -max

=item -not_in

=back




=head2 Date

=head2 Time

=head2 String

=head2 List


[[TODO : what happens with generic errors on structured domains (like
-min_size on Lists? ]]


=head2 Enum

=head2 CodeList

=head2 Struct


=head1 LAZY CONSTRUCTORS (CONTEXT DEPENDENCIES)



=head1 WRITING NEW DOMAIN CONSTRUCTORS






=head1 SEE ALSO

Other modules doing similar things :

Data::FormValidator
HTML::Widget::


=cut

=cut
