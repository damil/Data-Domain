#!perl

use Test::More tests => 1;

BEGIN {
  use_ok( 'Data::Domain', qw/:all/ );
}

diag( "Testing Data::Domain $Data::Domain::VERSION, Perl $], $^X" );


my $domain = Struct(
  anInt     => Int(-min => 3, -max => 18),
  aDate     => Date(-max => 'today'),
  aString   => String(-min_length => 2),
  anIntList => List(-min_size => 1, -all => Num),
  aStruct   => Struct(foo => String, bar => Num)
 );


my $data1 = {
  anInt     => 4,
  aDate     => '01.01.2001',
  aString   => 'toto',
  anIntList => [1 .. 6],
  aStruct   => {foo => 'foo', bar => 1234},
};

my $data2 = {
  anInt     => 1,
  aDate     => '09.09.2999',
  aString   => 't',
  anIntList => [ 'A' .. 'F'],
  aStruct   => {},
};

use Data::Dumper;

for my $data ($data1, $data2) {
  my $msgs = $domain->inspect($data);
  print Dumper($msgs);
}


