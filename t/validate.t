#!perl
use strict;
use warnings;
use Test::More;
use Test::NoWarnings;
use Data::Domain qw/:all/;
use Try::Tiny;


my $dom;
my $msg;

$dom = Struct(foo => Int, bar => Int(-default => 123), zim => List(-items => [String, String(-default => "qqq")], -default => [qw/a b c/]));

my $h = $dom->validate({foo => 456, zim => ["aaa"]});
my $expected = { bar => 123,
                 foo => 456,
                 zim => ['aaa', 'qqq'] };
is_deeply($h, $expected, "input valid and defaults filled");


ok( ! eval {$dom->validate({foo => "foo", bar => "bar"})}, "invalid input");
note explain $@;


done_testing;


__END__

todo
  - -default => sub ... (except for Coderef domain)
