use strict;
use warnings;

sub parse_input {
    my ($filename) = @_;
    open my $in, "<", $filename or die "failed to open file: $!";
    return <$in> =~ /./g;
}

sub process_diskmap_part1 {
    my (@diskmap) = @_;

    my $filecount = 0;
    my $fileblocks = 0;
    
    my $isfile = 1;
    foreach (@diskmap) {
        if ($isfile) {
            $filecount++;
            $fileblocks += $_;
        }
        $isfile = 1 - $isfile;
    }
    
    my @result = ();
    my $fileid_fw = 0;
    my $fileid_bw = $filecount - 1;
    my $filesize_bw = $diskmap[$fileid_bw * 2];
    
    $isfile = 1;
    OUTER: foreach (@diskmap) {
        if ($isfile) {
            push @result, ($fileid_fw) x $_;
            $fileid_fw++;
        } else {
            my $space = $_;
            while ($space > 0) {
                if ($filesize_bw <= $space) {
                    push @result, ($fileid_bw) x $filesize_bw;
                    $space -= $filesize_bw;
                    $filesize_bw = 0;
                } else {
                    push @result, ($fileid_bw) x $space;
                    $filesize_bw -= $space;
                    $space = 0;
                }
                if ($filesize_bw == 0) {
                    $fileid_bw--;
                    $filesize_bw = $diskmap[$fileid_bw * 2];
                }
            }
        }
        if (scalar @result >= $fileblocks) { last OUTER; }
        $isfile = 1-$isfile;
    }
    return @result[0..$fileblocks-1];
}

sub new_ll_node {
    my ($fileid, $offset, $size) = @_;

    return {
        fileid => $fileid,
        offset => $offset,
        size => $size,
    }
}

sub process_diskmap_part2 {
    my (@diskmap) = @_;

    # create a linked list of files
    my $ll_head;
    my $ll_tail;
    my %nodes_by_fileid = ();
    
    my $fileid = 0;
    my $isfile = 1;
    my $offset = 0;
    foreach (@diskmap) {
        if ($isfile) {
            my $ll_node = { fileid => $fileid, offset => $offset, size => $_, next => undef, prev => undef };
            $nodes_by_fileid{$fileid} = $ll_node;
            if (defined $ll_tail) {
                ${$ll_node}{'prev'} = $ll_tail;
                ${$ll_tail}{'next'} = $ll_node;
                $ll_tail = $ll_node;
            } else {
                $ll_head = $ll_node;
                $ll_tail = $ll_node
            }
            $fileid++;
        }
        $offset += $_;
        $isfile = 1 - $isfile;
    }

    my $numfiles = $fileid;
    for ($fileid = $numfiles - 1; $fileid >= 0; $fileid--) {
        my $filenode = $nodes_by_fileid{$fileid};
        my $filesize = ${$filenode}{'size'};

        # look for gap between files to find first location admitting enough space
        my $firstnode = $ll_head;
        my $secondnode;
        while (defined ${$firstnode}{'next'}) {
            $secondnode = ${$firstnode}{'next'};
            my $newoffset = ${$firstnode}{'offset'} + ${$firstnode}{'size'};
            my $gap = ${$secondnode}{'offset'} - $newoffset;

            if ($newoffset >= ${$filenode}{'offset'}) { last; }
            
            # found a suitable gap
            if ($gap >= $filesize) {
                if ($secondnode != $filenode) {
                    # remove the filenode from it's location
                    
                    if (defined ${$filenode}{'prev'}) {
                        ${${$filenode}{'prev'}}{'next'} = ${$filenode}{'next'};
                    }
                    if (defined ${$filenode}{'next'}) {
                        ${${$filenode}{'next'}}{'prev'} = ${$filenode}{'prev'};
                    }

                    # insert the filenode between first and second node
                    ${$firstnode}{'next'} = $filenode;
                    ${$secondnode}{'prev'} = $filenode;
                    ${$filenode}{'prev'} = $firstnode;
                    ${$filenode}{'next'} = $secondnode;
                }
                
                # update the files new offset
                ${$filenode}{'offset'} = $newoffset;
                last;
            }
            $firstnode = ${$firstnode}{'next'};
        }
    }

    my $filenode = $ll_head;
    my $checksum = 0;
    while (defined $filenode) {
        my $n = ${$filenode}{'size'};
        my $a = ${$filenode}{'offset'};
        $checksum += ${$filenode}{'fileid'} * ( $a * $n + ($n * ($n - 1)) / 2 );
        $filenode = ${$filenode}{'next'};
    }
    return $checksum;
}

my @diskmap = parse_input "input.txt";
my @result = process_diskmap_part1 @diskmap;

my $checksum = 0;
my $idx = 0;
foreach (@result) {
    $checksum += $_ * $idx;
    $idx++;
}
print "checksum = $checksum\n";

$checksum = process_diskmap_part2 @diskmap;
print "checksum part 2 = $checksum\n";
