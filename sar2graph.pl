#!/usr/bin/env perl

use strict;
use warnings;
use utf8;
use 5.010_001;

use IO::File;
STDOUT->autoflush(1);
STDERR->autoflush(1);

use Getopt::Long qw(:config posix_default no_ignore_case no_ignore_case_always);
use Pod::Usage;
use Data::Dumper;
BEGIN {
    sub p($) { ## no critic
        local $Data::Dumper::Indent    = 1;
        local $Data::Dumper::Deepcopy  = 1;
        local $Data::Dumper::Sortkeys  = 1;
        local $Data::Dumper::Terse     = 1;
        local $Data::Dumper::Useqq     = 1;
        local $Data::Dumper::Quotekeys = 0;
        my $d =  Dumper($_[0]);
        $d    =~ s/\\x{([0-9a-z]+)}/chr(hex($1))/ge;
        print STDERR $d;
    }
}

use Log::Minimal;
use Data::Validator;
use Carp;
use List::MoreUtils qw(zip);
use RRDs;
use Time::Piece;

my $Debug = 0;

my $RRD_Dir   = './rrd';
my $Graph_Dir = './img';
my $Graph_Width  = 255;
my $Graph_Height = 224;
my $Hostname;
my %Period = (
    start => 0,
    end   => 0,
);

my %Stat = (
    'CPU' => {
        id       => 'CPU',
        columns  => [],
        multiple => 1,
    },
    'proc/s' => {
        id       => 'Proc',
        columns  => [],
        multiple => 0,
    },
    'pswpin/s' => {
        id       => 'SwapPage',
        columns  => [],
        multiple => 0,
    },
    'pgpgin/s' => {
        id       => 'Paging',
        columns  => [],
        multiple => 0,
    },
    'tps' => {
        id       => 'IOPS',
        columns  => [],
        multiple => 0,
    },
    'frmpg/s' => {
        id       => 'MemoryPage',
        columns  => [],
        multiple => 0,
    },
    'kbmemfree' => {
        id       => 'MemoryUtil',
        columns  => [],
        multiple => 0,
    },
    'kbswpfree' => {
        id       => 'SwapUtil',
        columns  => [],
        multiple => 0,
    },
    'dentunusd' => {
        id       => 'Inode',
        columns  => [],
        multiple => 0,
    },
    'runq-sz' => {
        id       => 'Load',
        columns  => [],
        multiple => 0,
    },
    'TTY' => {
        id       => 'tty',
        columns  => [],
        multiple => 1,
    },
    'DEV' => {
        id       => 'BlockDevice',
        columns  => [],
        multiple => 1,
    },
    'IFACE' => {
        id       => 'Network',
        columns  => [],
        multiple => 1,
    },
    'IFACE_error' => {
        id       => 'NetworkError',
        columns  => [],
        multiple => 1,
    },
    'call/s' => {
        id       => 'NFS',
        columns  => [],
        multiple => 0,
    },
    'scall/s' => {
        id       => 'NFSD',
        columns  => [],
        multiple => 0,
    },
    'totsck' => {
        id       => 'SOCK',
        columns  => [],
        multiple => 0,
    },
);
my %Key_of = map { $Stat{$_}{id} => $_ } keys %Stat;
$Key_of{NetworkError} = 'IFACE';

my @Graph = (
    {
        title => 'CPU Utilization',
        vertical_label => '%',
        ds => ['CPU'],
        opt => [qw(--upper-limit 105 --rigid)],
        draw => sub {
            my($rrdfile, $cols) = @_;
            my @draw;
            state $color = {
                sys    => '#008080',
                usr    => '#c0c0c0',
                nice   => '#000080',
                iowait => '#f00000',
                steal  => '#EBF906',
                irq    => '#3D282A',
                soft   => '#F39034',
                guest  => '#d1a2f6',
                idle   => '#800080',
            };

            for my $col (@$cols) {
                my $ds_name = ds_name($col);
                push @draw, sprintf("AREA:%s%s:%s:STACK",
                                    $ds_name,
                                    ($color->{$ds_name} // '#ff0000'),
                                    sprintf("%-8s",$col),
                                );
                push @draw, sprintf("GPRINT:%s:MAX:%%4.1lf",
                                    $ds_name,
                                );
            }
            return @draw;
        },
    },
    {
        title => 'IOPS',
        vertical_label => 'IOPS',
        ds => ['tps'],
        draw => sub {
            my($rrdfile, $cols) = @_;
            my @draw;

            push @draw, "CDEF:wtpsm=wtps,-1,*";
            push @draw, sprintf("AREA:%s%s:%s",
                                'rtps',
                                '#c0c0c0',
                                'read ',
                            );
            push @draw, gprint('rtps');
            push @draw, sprintf("AREA:%s%s:%s",
                                'wtpsm',
                                '#800080',
                                'write',
                            );
            push @draw, gprint('wtps');

            return @draw;
        },
    },
    {
        title => 'Memory Utilization',
        vertical_label => '',
        ds => ['kbmemfree','kbswpfree'],
        base => 1024,
        draw => sub {
            my($rrdfile, $cols) = @_;
            my @draw;

            state $color = {
                bmemused => '#ffdd67',
                bbuffers => '#8a8ae6',
                bcached  => '#6060e0',
                bmemfree => '#80e080',
                bswpused => '#ff0000',
            };
            state $label = {
                bmemused => 'Used',
                bbuffers => 'Buffers',
                bcached  => 'Cached',
                bmemfree => 'Free',
                bswpused => 'Swap',
            };

            for my $col (qw(kbmemused kbmemfree kbbuffers kbcached kbswpused)) {
                my $cdef = $col;
                $cdef =~ s/^kb/b/;
                push @draw, sprintf("CDEF:%s=%s,1024,*",
                                    $cdef,
                                    $col,
                                );
            }


            for my $col (qw(bmemused bmemfree)) {
                my $ds_name = ds_name($col);
                push @draw, sprintf("AREA:%s%s:%s:STACK",
                                    $ds_name,
                                    ($color->{$ds_name} // '#ff0000'),
                                    sprintf("%-8s",$label->{$col}),
                                );
                push @draw, gprint($ds_name);
            }
            for my $col (qw(bbuffers bcached)) {
                my $ds_name = ds_name($col);
                push @draw, sprintf("AREA:%s%s:%s",
                                    $ds_name,
                                    ($color->{$ds_name} // '#ff0000'),
                                    sprintf("%-8s",$label->{$col}),
                                );
                push @draw, gprint($ds_name);
            }
            for my $col (qw(bswpused)) {
                my $ds_name = ds_name($col);
                push @draw, sprintf("LINE1:%s%s:%s",
                                    $ds_name,
                                    ($color->{$ds_name} // '#ff0000'),
                                    sprintf("%-8s",$label->{$col}),
                                );
                push @draw, gprint($ds_name);
            }

            return @draw;
        },
    },
    {
        title => 'IOPS',
        vertical_label => 'IOPS',
        ds => ['DEV'],
        draw => sub {
            my($rrdfile, $cols) = @_;
            my @draw;

            push @draw, sprintf("LINE:%s%s:%s",
                                'tps',
                                '#800080',
                                'IOPS',
                            );
            push @draw, gprint('tps');

            return @draw;
        },
    },
    {
        title => 'IO Transfer',
        vertical_label => 'bytes/s',
        ds => ['DEV'],
        base => 1024,
        draw => sub {
            my($rrdfile, $cols) = @_;
            my @draw;

            push @draw, "CDEF:rd=rd_sec_s,512,*";
            push @draw, "CDEF:wr=wr_sec_s,512,*";
            push @draw, "CDEF:wrm=wr,-1,*";

            push @draw, sprintf("AREA:%s%s:%s",
                                'rd',
                                '#c0c0c0',
                                'read ',
                            );
            push @draw, gprint('rd');
            push @draw, sprintf("AREA:%s%s:%s",
                                'wrm',
                                '#800080',
                                'write',
                            );
            push @draw, gprint('wr');

            return @draw;
        },
    },
    {
        title => 'IO Util',
        vertical_label => '%',
        ds => ['DEV'],
        opt => [qw(--upper-limit 100 --rigid)],
        draw => sub {
            my($rrdfile, $cols) = @_;
            my @draw;

            push @draw, sprintf("AREA:%s%s:%s",
                                'util',
                                '#f00000',
                                '%util',
                            );
            push @draw, gprint('util');

            return @draw;
        },
    },
    {
        title => 'Network packets',
        vertical_label => 'packets/s',
        ds => ['IFACE'],
        draw => sub {
            my($rrdfile, $cols) = @_;
            my @draw;

            push @draw, sprintf("LINE:%s%s:%s",
                                'rxpck_s',
                                '#00C000',
                                'receive ',
                            );
            push @draw, gprint('rxpck_s');
            push @draw, sprintf("LINE:%s%s:%s",
                                'txpck_s',
                                '#0000FF',
                                'transmit',
                            );
            push @draw, gprint('txpck_s');

            return @draw;
        },
    },
    {
        title => 'Network traffic',
        vertical_label => 'bps',
        ds => ['IFACE'],
        base => 1024,
        draw => sub {
            my($rrdfile, $cols) = @_;
            my @draw;

            push @draw, "CDEF:rx=rxkB_s,8192,*";
            push @draw, "CDEF:tx=txkB_s,8192,*";

            push @draw, sprintf("LINE:%s%s:%s",
                                'rx',
                                '#00C000',
                                'receive ',
                            );
            push @draw, gprint('rx');
            push @draw, sprintf("LINE:%s%s:%s",
                                'tx',
                                '#0000FF',
                                'transmit',
                            );
            push @draw, gprint('tx');

            return @draw;
        },
    },
    {
        title => 'NFS client',
        vertical_label => 'RPC/s',
        ds => ['call/s'],
        draw => sub {
            my($rrdfile) = @_;
            my @draw;

            state $color = {
                read_s   => '#00C000',
                write_s  => '#0000FF',
                access_s => '#f00000',
                getatt_s => '#d1a2f6',
            };

            my $cols = [qw(read/s write/s access/s getatt/s)];

            for my $col (@$cols) {
                my $ds_name = ds_name($col);
                my $line = $ds_name =~ /(?:read|write)/ ? 'LINE2' : 'LINE1';
                push @draw, sprintf("${line}:%s%s:%s",
                                    $ds_name,
                                    ($color->{$ds_name} // '#ff0000'),
                                    sprintf("%-8s",$col),
                                );
                push @draw, gprint($ds_name);
            }

            return @draw;
        },
    },

);

MAIN: {
    my $target_host = '';

    my %arg;
    GetOptions(
        \%arg,
        'sar-file|f=s',
        'start|s=s',
        'end|e=s',
        # 'target|t=s',
        # 'interval|i=f',
        'debug|d+' => \$Debug,
        'help|h|?' => sub { pod2usage(-verbose=>1) }) or pod2usage();
    $ENV{LM_DEBUG} = 1 if $Debug;
    my $opt_rule = Data::Validator->new(
        'sar-file' => 'Str',
        'start'    => { isa => 'Str', default => '', },
        'end'      => { isa => 'Str', default => '', },
        # interval => { isa => 'Num', default => $Interval },
        # url    => { isa => 'Str', xor => [qw(schema host)] },
        # schema => { isa => 'Str', default => 'http' },
        # qs     => { isa => 'Str', optional => 1, },
    )->with('NoThrow');
    my $opt = $opt_rule->validate(%arg);
    pod2usage(join("\n", map {$_->{message}} @{$opt_rule->clear_errors}))
        if $opt_rule->has_errors;

    debugf("opt: %s", ddf($opt));

    unless (-f $opt->{'sar-file'}) {
        pod2usage("No such sar file: $opt->{'sar-file'}");
    }

    my($hostinfo, $stat) = parse_sar_file($opt->{'sar-file'});
    debugf('hostinfo: %s', ddf($hostinfo));
    $Hostname = $hostinfo->{hostname};

    my %data_period;
    for my $t (qw(start end)) {
        if ($opt->{$t}) {
            $Period{$t} = localtime->strptime($opt->{$t}, '%Y-%m-%d %H:%M:%S')->epoch;
        } else {
            unless (%data_period) {
                for my $id (keys %{$stat}) {
                    my $s;
                    if (ref($stat->{$id}) eq 'HASH') {
                        for my $key (keys %{ $stat->{$id} }) {
                            $s = $stat->{$id}{$key} and last;
                        }
                    } else {
                        $s = $stat->{$id};
                    }
                    $data_period{start} = localtime->strptime($hostinfo->{date}.' '.$s->[0]{time},'%Y-%m-%d %H:%M:%S')->epoch;
                    $data_period{end} = localtime->strptime($hostinfo->{date}.' '.$s->[-1]{time},'%Y-%m-%d %H:%M:%S')->epoch;
                }
            }
            $Period{$t} = $data_period{$t};
        }
    }
    infof("%s - %s", localtime($Period{start})->datetime, localtime($Period{end})->datetime);

    process_stat($hostinfo, $stat);

    infof('finished');
    exit 0;
}

sub parse_sar_file {
    my $sar_file = shift;
    my $hostinfo = {
        os       => '',
        kernel   => '',
        hostname => '',
        date     => '',
        arch     => '',
    };
    my $stat = {};
    infof 'Parse sar file';

    open my $fh, '<', $sar_file or die $!;
    {
        my $line = <$fh>;
        my @e = split /\s+/, $line;
        $hostinfo->{os}       = $e[0];
        $hostinfo->{kernel}   = $e[1];
        $hostinfo->{hostname} = $e[2];
        $hostinfo->{hostname} = $1 if $e[2] =~ m{^\((.+)\)$};
        my @mdy = split m{/}, $e[3];
        $hostinfo->{date} = sprintf "%04d-%02d-%02d", $mdy[2]+2000, $mdy[0], $mdy[1];
        $hostinfo->{arch}     = $e[4];
    }

    my %cur;
    while (my $line = <$fh>) {
        chomp $line;
        next if $line =~ /^$/;
        my $e = [ split /\s+/, $line ];
        if (my $s = is_header($e)) {
            %cur = %{ $s };
        } else {
            next unless $e->[0] =~ /^\d{2}:\d{2}:\d{2}$/;
            if ($cur{multiple}) {
                push @{ $stat->{ $cur{id} }{$e->[1]} }, +{
                    zip @{ $cur{columns} }, @$e
                };
            } else {
                push @{ $stat->{ $cur{id} } }, +{
                    zip @{ $cur{columns} }, @$e
                };
            }
        }
    }
    close $fh;



    return ($hostinfo, $stat);
}

sub is_header {
    my $e = shift;
    my $key = $e->[1];
    if ($key eq 'IFACE') {
        if (grep /rxerr/, @$e) {
            $key = 'IFACE_error';
        }
    }
    if (exists $Stat{$key}) {
        $Stat{$key}{columns} = [ 'time', @{$e}[1 .. $#{$e}] ];
        return $Stat{$key};
    } else {
        return;
    }
}

sub process_stat {
    my($hostinfo, $stat) = @_;

    for my $d ($RRD_Dir, $Graph_Dir) {
        unless (-d $d) {
            mkdir $d or die $!;
        }
    }

    for my $id (sort keys %$stat) {
        my $key = $Key_of{$id};
        infof "$id ($key)";

        if ($Stat{$key}{multiple}) {
            for my $subkey (sort keys %{ $stat->{$id} }) {
                my $rrdfile = rrdfile($id,
                                      $key,
                                      $subkey,
                                  );
                my @cols = @{ $Stat{$key}{columns} };
                shift @cols; shift @cols;
                store_stat($rrdfile, $stat->{$id}{$subkey}, $id, $key, \@cols, $hostinfo->{date});
            }
        } else {
            my $rrdfile = rrdfile($id,
                                  $key,
                              );
            my @cols = @{ $Stat{$key}{columns} };
            shift @cols;
            store_stat($rrdfile, $stat->{$id}, $id, $key, \@cols, $hostinfo->{date});
        }
    }

    for my $graph (@Graph) {
        if ($Stat{ $graph->{ds}[0] }{multiple}) {
            my $key = $graph->{ds}[0];
            my $id = $Stat{$key}{id};
            my @subkey = keys %{ $stat->{$id} };
            my @cols = @{ $Stat{$key}{columns} };
            shift @cols; shift @cols;
            for my $subkey (@subkey) {
                my(@def, @draw);

                my $rrdfile = rrdfile($id, $key, $subkey);
                push @def, generate_def($rrdfile, [ @cols ]);
                push @draw, ref($graph->{draw}) eq 'CODE' ? $graph->{draw}($rrdfile, [ @cols ]) : generate_draw($rrdfile, [ @cols ]);
                draw_graph(
                    imgfile($graph->{title}, $subkey),
                    $graph->{title}.' - '.$subkey,
                    $graph,
                    \@def,
                    \@draw,
                );
            }
        } else {
            my(@def, @draw, @cols);
            for my $key (@{ $graph->{ds} }) {
                my $id = $Stat{$key}{id};
                my @ds_cols = @{ $Stat{$key}{columns} };
                shift @ds_cols;
                push @cols, @ds_cols;
                my $rrdfile = rrdfile($id, $key);

                push @def, generate_def($rrdfile, [ @ds_cols ]);
            }
            {
                my $key = $graph->{ds}[0];
                my $id = $Stat{$key}{id};
                my $rrdfile = rrdfile($id, $key);
                push @draw, ref($graph->{draw}) eq 'CODE' ? $graph->{draw}($rrdfile, [ @cols ]) : generate_draw($rrdfile, [ @cols ]);
            }
            draw_graph(
                imgfile($graph->{title}),
                $graph->{title},
                $graph,
                \@def,
                \@draw,
            );
        }
    }
}

sub store_stat {
    my($rrdfile, $stat, $id, $key, $cols, $date) = @_;

    if (-f $rrdfile) {
        unlink $rrdfile or die $!;
    }

    my @ds;
    for my $col (@$cols) {
        my $ds_name = ds_name($col);
        push @ds, sprintf('DS:%s:GAUGE:900:0:U',
                          $ds_name,
                      );
    }

    RRDs::create(
        $rrdfile,
        '--start', localtime->strptime($date.' 00:00:00', '%Y-%m-%d %H:%M:%S')->epoch,
        '--step', 30,
        @ds,
        "RRA:AVERAGE:0.5:1:3000",
        "RRA:AVERAGE:0.5:4:2300", #  2min で 3.19 day
        "RRA:AVERAGE:0.5:20:1100", # 10min で 7.64 day
        "RRA:AVERAGE:0.5:120:800", #  1h で 33.3 day
        "RRA:AVERAGE:0.5:1440:2000", # 12h で 1000 day
        "RRA:MAX:0.5:1:3000",
        "RRA:MAX:0.5:4:2300",   #  2min で 3.19 day
        "RRA:MAX:0.5:20:1100",  # 10min で 7.64 day
        "RRA:MAX:0.5:120:800",  #  1h で 33.3 day
        "RRA:MAX:0.5:1440:2000", # 12h で 1000 day
    ) or die RRDs::error;

    for my $s (@$stat) {
        my $data = join(':',
                        localtime->strptime($date.' '.$s->{time}, '%Y-%m-%d %H:%M:%S')->epoch,
                        map { ! defined $s->{$_} ? 'U' : $s->{$_}} @$cols,
                    );
        RRDs::update(
            $rrdfile,
            $data,
        ) or die RRDs::error;
    }
}

sub draw_graph {
    my($imgfile, $title, $graph, $def, $draw) = @_;
    infof("  draw: %s", $imgfile);

    debugf("def : %s", join(' ', @$def));
    debugf("draw: %s", join(' ', @$draw));

    RRDs::graph(
        $imgfile,
        '--slope-mode',
        '--start', $Period{start},
        '--end', $Period{end},
        '--title', $title,
        '--vertical-label', ($graph->{vertical_label} // ''),
        '--base', ($graph->{base} // '1000'),
        '--width', $Graph_Width,
        '--height', $Graph_Height,
        '--units', 'si',
        ($graph->{opt} ? @{ $graph->{opt} } : ()),
        @$def,
        @$draw,
    ) or die RRDs::error;

}

sub ds_name {
    my $ds_name = shift;
    $ds_name =~ s/[%]//g;
    $ds_name =~ s/[\/]/_/g;
    return $ds_name;
}

sub generate_def {
    my($rrdfile, $cols) = @_;
    my @def;
    for my $col (@$cols) {
        my $ds_name = ds_name($col);
        push @def, sprintf("DEF:%s=%s:%s:MAX",
                           $ds_name,
                           $rrdfile,
                           $ds_name,
                       );
    }
    return @def;
}

sub generate_draw {
    my($rrdfile, $cols) = @_;
    my @draw;

    my @colors = ( "#FF0000", "#0000FF", "#00FFFF", "#FF00FF", "#FFFF00", "#00FF00", "#000000", "#C0C0C0", "#FF8C00", "#8FBC8F" );

    for my $col (@$cols) {
        my $ds_name = ds_name($col);
        push @draw, sprintf("LINE1:%s%s:%s:STACK",
                            $ds_name,
                            shift(@colors),
                            sprintf("%-8s",$col),
                        );
        push @draw, sprintf("GPRINT:%s:MAX:%%1.2lf",
                            $ds_name,
                        );
    }
    return @draw;
}

sub rrdfile {
    my($id, $key, $subkey) = @_;
    if ($subkey) {
        sprintf("%s/%s-%s-%s_%s.rrd",
                $RRD_Dir,
                $Hostname,
                $id,
                ds_name($key),
                ds_name($subkey),
            );
    } else {
        sprintf("%s/%s-%s-%s.rrd",
                $RRD_Dir,
                $Hostname,
                $id,
                ds_name($key),
            );
    }
}

sub imgfile {
    my($id, $subkey) = @_;
    $id =~ s/ /_/g;
    if ($subkey) {
        sprintf("%s/%s-%s-%s.png",
                $Graph_Dir,
                $Hostname,
                $id,
                ds_name($subkey),
            );
    } else {
        sprintf("%s/%s-%s.png",
                $Graph_Dir,
                $Hostname,
                $id,
            );
    }
}

sub gprint {
    my($ds_name) = @_;
    my @gp;
# GPRINT:my1:LAST:Cur\: %4.1lf%s
# GPRINT:my1:AVERAGE:Ave\: %4.1lf%s
# GPRINT:my1:MAX:Max\: %4.1lf%s
# GPRINT:my1:MIN:Min\: %4.1lf%s\l
    for my $e (
        [qw(LAST Cur)],
        [qw(AVERAGE Avg)],
        [qw(MAX Max)],
        [qw(MIN Min)],
    ) {
        push @gp, sprintf('GPRINT:%s:%s:%s\: %%5.1lf%%s',
                          $ds_name,
                          $e->[0],
                          $e->[1],
                      );
    }
    $gp[-1] .= '\l';
    return @gp;
}

__END__

=encoding utf-8

=head1 NAME

B<program_name> - abstract

=head1 SYNOPSIS

B<program_name>
file
[B<-r> arg]
[B<-e> days]
[B<-n>]
[B<-l> limit]
[B<-u> clock_time]
[B<-d> | B<--debug>]

B<program_name> B<-h> | B<--help> | B<-?>

  $ program_name file > out

=head1 DESCRIPTION

このプログラムは...

=head1 OPTIONS

=over 4

=item B<-r> arg, B<--require> arg

=item B<-e> days, B<--expire> days

=item B<-n>, B<--dry-run>

=item B<-d>, B<--debug>

increase debug level.
-d -d more verbosely.

=back

=head1 EXIT STATUS

exit status.

=head1 RETURN VALUE

What the program or function returns, if successful. This section can
be omitted for programs whose precise exit codes aren't important,
provided they return 0 on success as is standard.  It should always be
present for functions.

=head1 ERRORS

errors.

=head1 DIAGNOSTICS

All possible messages the program can print out--and what they mean.
You may wish to follow the same documentation style as the Perl
documentation; see perldiag(1) for more details (and look at the POD
source as well).

=head1 EXAMPLES

=head2 ex1

ext1...

=head1 ENVIRONMENT

=over 6

=item HOME

Used to determine the user's home directory.

=back

=head1 FUNCTIONS

=over 4

=item B<f1>($a,$b)

call from:
call out :
in :
out:

desc.

=back

=head1 FILES

=over 4

=item F</path/to/config.ph>

設定ファイル。

=back

=head1 CAVEATS

Things to take special care with, sometimes called WARNINGS.

=head1 BUGS

Things that are broken or just don't work quite right.

=head1 RESTRICTIONS

Bugs you don't plan to fix.  :-)

=head1 NOTES

Miscellaneous commentary.

=head1 SEE ALSO

L<Module::Hoge|Module::Hoge>,
ls(1), cd(1)

=head1 AUTHOR

HIROSE, Masaaki E<lt>hirose31 _at_ gmail.comE<gt>

=head1 LICENSE

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut

# for Emacsen
# Local Variables:
# mode: cperl
# cperl-indent-level: 4
# cperl-close-paren-offset: -4
# cperl-indent-parens-as-block: t
# indent-tabs-mode: nil
# coding: utf-8
# End:

# vi: set ts=4 sw=4 sts=0 et ft=perl fenc=utf-8 ff=unix :
