#!/usr/bin/perl

# info2def2.pl
# 
# Parses javadoc-like comments from C++ files and writes them into well-formed
# (hopefully) XML NodeClass definitions, that can then be used by Overflow to
# extract info about the nodes.
#
# Usage:
# Takes a file as input and prints out the XML, so the read the info from 
# to_read.cc and output it to nice_xml.def you would need to do something like:
# 'perl info2def2.pl to_read.cc > nice_xml.def.'


# hash_copy
#
# a little function to copy over a hash since I couldn't figure out how
# to do this in perl.
sub hash_copy {
  my $to_copy = shift(@_);
  my $new_hash = {};
  
  while (($key, $value) = each (%{$to_copy})) {
    $$new_hash{$key} = $value;
  }
  return $new_hash;
}

print "<?xml version=\"1.0\"?>\n";
print "<Definitions>\n";

# data structures to store the info for a node. They look like the following:
# 
# info{name} -> name
# info{category} -> category
# info{description} -> description
# info{inputs} -> list of input info
#      info{inputs}[n]{input_name} -> name
#      info{inputs}[n]{input_type} -> type
#      info{inputs}[n]{input_description} -> description
# info{outputs} -> list of output info
#      info{outputs}[n]{output_name} -> name
#      info{outputs}[n]{output_type} -> type
#      info{outputs}[n]{output_description} -> description
# info{params} -> list of parameter info
#      info{params}[n]{parameter_name} -> name
#      info{params}[n]{parameter_type} -> type
#      info{params}[n]{parameter_value} -> value
#      info{params}[n]{parameter_description} -> description

$default_input =  
  {input_name => "No Name", 
   input_type => "any", 
   input_description => "No Description Available"};

$default_output = 
  {output_name => "No Name", 
   output_type => "any", 
   output_description => "No Description Available"};

$default_param = 
  {parameter_name => "No Name", 
   parameter_type => "any", 
   parameter_value => "", 
   parameter_description => "No Description Available"};

%node_info = 
  (name => "No Name", 
   category => "No Category", 
   description => "No Description Available", 
   inputs => [], 
   outputs => [], 
   params => []);

# the last info parsed, so we can deal with carryovers.
$last_info = "";
$last_name = "";

# flag so we know if we are in a javadoc comment
$in_comment = 0;

# assign the inputs and outputs to the default value
$current_input = hash_copy($default_input);
$current_output =  hash_copy($default_output);
$current_param = hash_copy($default_param);
	      
# read through the file
while (<>)
  {
    # if we are in a comment, look for the information to parse.
    if ($in_comment == 1) {

      # look for a '* @something info', then we need to process the item.
      if (/\* (@[a-zA-Z:_0-9]+) ([a-zA-Z:_0-9 .]+)/) {  
	if ($1 eq '@name') {
	  #print '@name : ', $1, "\n";
	  $node_info{substr($1, 1)} = $2;
	}
	elsif ($1 eq '@category') {
	  $node_info{substr($1, 1)} = $2;
	}
	elsif ($1 eq '@description') {
	  $node_info{substr($1, 1)} = $2;
	}
	# start of a new input
	elsif ($1 eq '@input_name') {
	  # check if we have an old input we need to append
	  if ($$current_input{input_name} ne 'No Name') {
	    # add the current input to the inputs
	    push @{$node_info{inputs}}, $current_input;
	    # clear the input
	    $current_input = hash_copy($default_input);
	  }
	  $$current_input{substr($1, 1)} = $2;
	}
	elsif ($1 eq '@input_type') {
	  $$current_input{substr($1, 1)} = $2;
	}
	elsif ($1 eq '@input_description') {
	  $$current_input{substr($1, 1)} = $2;
	}
	# start of a new output
	elsif ($1 eq '@output_name') {
	  # check if we have an old output we need to append
	  if ($$current_output{output_name} ne 'No Name') {
	    # add the current output to the outputs
	    push @{$node_info{outputs}}, $current_output;
	    # clear the info
	    $current_output = hash_copy($default_output);
	  }
	  
	  $$current_output{substr($1, 1)} = $2;
	}
	elsif ($1 eq '@output_type') {
	  $$current_output{substr($1, 1)} = $2;
	}
	elsif ($1 eq '@output_description') {
	  $$current_output{substr($1, 1)} = $2;
	}
	# start of a new parameter
	elsif ($1 eq '@parameter_name') {
	  # check if we have an old parameter to append to the info
	  if ($$current_param{parameter_name} ne 'No Name') {
	    # add the current parameter to the parameter list
	    push @{$node_info{params}}, $current_param;
	    # clear the parameter info
	    $current_param = hash_copy($default_param);
	  }
	  $$current_param{substr($1, 1)} = $2;
	}
	elsif ($1 eq '@parameter_type') {
	  $$current_param{substr($1, 1)} = $2;
	}
	elsif ($1 eq '@parameter_value') {
	  $$current_param{substr($1, 1)} = $2;
	}
	elsif ($1 eq '@parameter_description') {
	  $$current_param{substr($1, 1)} = $2;
	}
	else {
	  die 'Unrecognized "@" tag: ', $1, "\n";
	}
	# set the added information as the last info added
	$last_name = substr($1, 1);
	$last_info = $2;
      }

      elsif (/END\*\// ) { # look for the trailing '*/' 

	if ($$current_input{input_name} ne 'No Name') {
	  push @{$node_info{inputs}}, $current_input;
	}
	if ($$current_output{output_name} ne 'No Name') {
	  push @{$node_info{outputs}}, $current_output;
	}
	if ($$current_param{parameter_name} ne 'No Name') {
	  push @{$node_info{params}}, $current_param;
	}
	
	print "  <NodeClass name=\"$node_info{name}\" ",
	"category =\"$node_info{category}\">\n";
	
	foreach $input (@{$node_info{inputs}}) {
	  print "    <Input name=\"$$input{input_name}\" ",
	  "type=\"$$input{input_type}\">\n";
	  print "        $$input{input_description}\n";
	  print "    </Input>\n";
	}
	
	foreach $output (@{$node_info{outputs}}) {
	  print "    <Output name=\"$$output{output_name}\" ",
	  "type=\"$$output{output_type}\">\n";
	  print "       $$output{output_description}\n";
	  print "    </Output>\n";
	}
	
	foreach $param (@{$node_info{params}}) {
	  print "    <Parameter name=\"$$param{parameter_name}\" ",
	  "type=\"$$param{parameter_type}\" ",
	  "value=\"$$param{parameter_value}\">\n";
	  print "        $$param{parameter_description}\n";
	  print "    </Parameter>\n";
	}
	
	print "    <Description>\n";
	print "       $node_info{description}\n";
	print "    </Description>\n";
	print "  </NodeClass>\n";
	
	# the last info parsed, so we can deal with carryovers.
	$last_info = "";
	$last_name = "";
	
	# flag so we know if we are in a javadoc comment
	$in_comment = 0;
	
	# assign the inputs and outputs to the default value
	$current_input = "";
	$current_output =  "";
	$current_param = "";
	
	$current_input = hash_copy($default_input);
	$current_output =  hash_copy($default_output);
	$current_param = hash_copy($default_param);
	%node_info=0;
	$in_comment = 0;
      }
      # line spill over from the previous comment. Need to add this
      # info to the previous value
      elsif (/\* ([a-zA-Z:_0-9 .]+)/) {
	$new_info = $last_info . " " . $1;
	# if it is input info add it to the current_input
	if (substr($last_name, 0, 5) eq 'input') {
	  $$current_input{$last_name} = $new_info;
	}
	# if it is an output, add it to the current output
	elsif (substr($last_name, 0, 6) eq 'output') {
	  $$current_output{$last_name} = $new_info;
	}
	# if it is a param, add it to the current param
	elsif (substr($last_name, 0, 9) eq 'parameter') {
	  $$current_param{$last_name} = $new_info;
	}
	# otherwise, we can add it directly to the node info
	else {
	  $node_info{$last_name} = $new_info;
	}
	# keep expanding last_info for multiple carryovers
	$last_value = $new_info;
      }
      elsif (/\*/) {
	# print "blank line \n";
      }
  
    }
    # if we aren't in a comment, then keep looking for the beginning
    elsif (/\/\*Node/) { # look for the initial '/*Node'
      # print "comment\n";
      $in_comment = 1;
    }

  }

# if we have inputs, outputs and parameters that haven't been added, add 'em



# once we've got to the end, print out the XML


print "</Definitions>\n";

