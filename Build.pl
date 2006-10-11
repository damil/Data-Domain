use strict;
use warnings;
use Module::Build;

my $builder = Module::Build->new(
    module_name         => 'Data::Domain',
    license             => 'perl',
    dist_author         => 'Laurent Dami <laurent.dami@justice.ge.ch>',
    dist_version_from   => 'lib/Data/Domain.pm',
    build_requires => {
        'Test::More'   => 0,
        'Scalar::Util' => 0,
        'Date::Calc'   => 0,
    },
    add_to_cleanup      => [ 'Data-Domain-*' ],
);

$builder->create_build_script();
