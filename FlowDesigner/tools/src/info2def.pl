#!/usr/bin/perl

print "<?xml version=\"1.0\"?>\n";
print "<Definitions>\n";


while (<>)
  {
    #if (/NODE_INFO\(\ *([a-zA-Z_])\ *,.*/)
    if (/NODE_INFO\ *\(([a-zA-Z_0-9]+)\ *,\ *\"([a-zA-Z:_0-9]+)\"\ *,\ *\"([a-zA-Z:_0-9]*)\"\ *,\ *\"([a-zA-Z:_0-9]*)\"\ *,\ *\"([a-zA-Z:_0-9]*)\".*\)/)
      {
	
	#print $1, ":", $2, ":", $3, ":", $4, ":", $5, "\n";
	$name = $1;
	$category = $2;
	$inputs = $3;
	$outputs = $4;
	$params = $5;

	print "   <NodeClass classname=\"$name\" category=\"$category\">\n";
	if ($inputs)
	  {
	    @in = split(/:/, $inputs);
	    for ($i=0;$i<=$#in;$i++)
	      {
		print "      <Input name=\"$in[$i]\" type=\"ANY\">\n";
		print "         Info for $in[$i] not available\n";
		print "      </Input>\n";
	      }
	  }

	if ($outputs)
	  {
	    @out = split(/:/, $outputs);
	    for ($i=0;$i<=$#out;$i++)
	      {
		print "      <Output name=\"$out[$i]\" type=\"ANY\">\n";
		print "         Info for $out[$i] not available\n";
		print "      </Output>\n";
	      }
	  }
	if ($params)
	  {
	    @par = split(/:/, $params);
	    for ($i=0;$i<=$#par;$i++)
	      {	    
		print "      <Parameter name=\"$par[$i]\" type=\"ANY\" opt=\"optional\">\n";
		print "         Info for $par[$i] not available\n";
		print "      </Parameter>\n";
	      }
	  }
	print "      <Description>\n";
	print "         Description not available\n";
	print "      </Description>\n";
	print "   </NodeClass>\n";
      }
  }

print "</Definitions>\n";
