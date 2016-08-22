#!/usr/bin/env perl

my $lineno = 0;
print qq!<table class="schedule">\n!;
while (<STDIN>) {
  chop;

  my ($week,$weekday,$date,$topic,$reading,$hw) = csv_split($_);
    
  if ($weekday ne "") {
    print qq!  <tr class="meeting weekday$weekday">\n!;
  }
  else {
    print qq!  <tr>\n!;
  }

  my $tcol = $lineno == 0 ? "th" : "td";
  {
  print <<END_TEXT
    <$tcol>$week</$tcol>
    <$tcol class="weekday">$weekday</$tcol>
    <$tcol>$date</$tcol>
    <$tcol>$part</$tcol>
    <$tcol>
      $topic
    </$tcol>
    <$tcol>
      $reading
    </$tcol>
    <$tcol>
      $hw
    </$tcol>
END_TEXT
  }

  print "  </tr>\n";
  $lineno++;
}
print qq!</table>\n!;


sub csv_split {
  my ($line) = @_;
  my @fields = ();

  while ($line ne "") {
    my $f;

    if ($line =~ /^\"/) {
      $line =~ s/^\"//;
      ($f, $line) = split /\"/, $line, 2;
    }
    else {
      ($f, $line) = split /,/, $line, 2;
    }

    push @fields, $f;
  }

  return @fields;
}
