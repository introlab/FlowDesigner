#!/usr/bin/perl

while (<>)
  {
    if (/<dia:layer name="(.*)" visible="true">/)
      {
	$net_name = $1;
	parse_net($net_name);
      }
  }

sub parse_net {
  $name = $_[0];
  $defined = 0;
  $nb_nodes=0;
  $nb_connect=0;
  $curr_net=0;
  $net_input=0;
  $net_output=0;
  $net_condition=0;
  while (<>)
    {
      if (/<\/dia:layer>/)
	{
	  goto EndOfNet;
	}
      if (/<dia:object type.*Function.*id="(.*)">/)
	{
	  parse_node($1);
	}
      if (/<dia:object type.*Flow.*id="(.*)">/)
	{
	  parse_connexion($1);
	}
    }
  
 EndOfNet:
set_connect();
  if ($net_condition)
    {
      print "Iterator: $name {\n\n";
    } else {
      print "Network: $name {\n\n";
    }
  for ($i=0;$i<=$#curr_net;$i++)
    {
      $name = $curr_net[$i]{"name"};
      $type = $curr_net[$i]{"type"};
      print "   <node: $name> <type: $type>\n";

      for ($j=0;$j<$curr_net[$i]{"nb_inputs"};$j++)
	{
	  print "   ", $curr_net[$i]{"inputs"}[$j], "\n";
	}

      for ($j=0;$j<$curr_net[$i]{"nb_params"};$j++)
	{
	  $pname = $curr_net[$i]{"param"}[$j][0];
	  $pval = $curr_net[$i]{"param"}[$j][1];
	  print "   <param: $pname, $pval>\n";;
	}
      print "\n";
    }
  
  for ($i=0;$i<$nb_nodes;$i++)
    {
      if ($curr_net[$i]{"id"} eq $net_input)
	{
	  $name = $curr_net[$i]{"name"};
	  printf "   <netInput: $name>\n";
	}
    }
  for ($i=0;$i<$nb_nodes;$i++)
    {
      if ($curr_net[$i]{"id"} eq $net_output)
	{
	  $name = $curr_net[$i]{"name"};
	  printf "   <netOutput: $name>\n";
	}
    }
  if ($net_condition)
    {
      for ($i=0;$i<$nb_nodes;$i++)
	{
	  if ($curr_net[$i]{"id"} eq $net_condition)
	    {
	      $name = $curr_net[$i]{"name"};
	      printf "   <netCondition: $name>\n";
	    }
	}
    }
  print "}\n\n";

}

sub parse_node {
  $curr_node_id = $_[0];
  #print "parsing node id $id\n";
  while (<>)
    {
      if (/<\/dia:object>/)
	{
	  goto EndOfNode;
	}
      if (/<dia:string>\#(.*)/)
	{
	  $curr_net[$nb_nodes]{"name"}=$1;
	  parse_node_indo();
	  $nb_nodes++;
	}
    }
 EndOfNode:

      #if (/<dia:string>\#(.*)/)
}

sub parse_connexion {
  $id = $_[0];
  #print "parsing connexion id $id\n";
  while (<>)
    {
      if (/<\/dia:object>/)
	{
	  goto EndOfConnexion;
	}
      if (/<dia:string>\#netInput\#<\/dia:string>/)
	{
	  while (<>)
	    {
	      if (/<dia:connection handle="1" to="(.*)" connection.*>/)
		{$net_input=$1;}
	      if (/<\/dia:object>/)
		{goto EndOfConnexion;}
	    }
	  return;
	}
      if (/<dia:string>\#netOutput\#<\/dia:string>/)
	{
	  while (<>)
	    {
	      if (/<dia:connection handle="0" to="(.*)" connection.*>/)
		{$net_output=$1;}
	      if (/<\/dia:object>/)
		{goto EndOfConnexion;}
	    }
	  return;
	}
      if (/<dia:string>\#netCondition\#<\/dia:string>/)
	{
	  while (<>)
	    {
	      if (/<dia:connection handle="0" to="(.*)" connection.*>/)
		{$net_condition=$1;}
	      if (/<\/dia:object>/)
		{goto EndOfConnexion;}
	    }
	  return;
	}
      if (/<dia:string>\#\#<\/dia:string>/)
	{
	  $net_connect[$nb_connect]{"input"}="INPUT";
	  $net_connect[$nb_connect]{"output"}="OUTPUT";
	  goto next_connect;
	}
      if (/<dia:string>\#(.*)/)
	{
	  $net_connect[$nb_connect]{"output"}=$1;
	}
      if (/(.*)\#<\/dia:string>/)
	{
	  $net_connect[$nb_connect]{"input"}=$1;
	}
      if (/<dia:connection handle="0" to="(.*)" connection.*>/)
	{
	  $net_connect[$nb_connect]{"from_node_id"}=$1;
	}
      if (/<dia:connection handle="1" to="(.*)" connection.*>/)
	{
	  $net_connect[$nb_connect]{"to_node_id"}=$1;
	}
    next_connect:

    }
 EndOfConnexion:

  $nb_connect++;

  #print "INPUT = $to_node_id, OUTPUT = $from_node_id\n";
  #print "INPUT = $input, OUTPUT = $output\n";
}

sub set_connect {
  for ($conn=0;$conn < $nb_connect; $conn++)
    {
      $input=$net_connect[$conn]{"input"};
      $output=$net_connect[$conn]{"output"};
      $to_node_id = $net_connect[$conn]{"to_node_id"};
      $from_node_id = $net_connect[$conn]{"from_node_id"};
      
      for ($i=0;$i<$nb_nodes;$i++)
	{
	  if ($curr_net[$i]{"id"} eq $to_node_id)
	    {
	      for ($j=0;$j<$nb_nodes;$j++)
		{
		  if ($curr_net[$j]{"id"} eq $from_node_id)
		    {
		      $nb_inputs = $curr_net[$i]{"nb_inputs"};
		      $in_name = $curr_net[$j]{"name"};
		      $curr_net[$i]{"inputs"}[$nb_inputs]="<input: $input, $in_name, $output>";
		      #print $curr_net[$i]{"inputs"}[$nb_inputs], "\n";
		      $curr_net[$i]{"nb_inputs"}++;
		      goto connect_find_out;
		    }
		}
	      
	    }
	}
    connect_find_out:
    }
}

sub parse_node_indo {
  $istype=1;
  $param_count=0;
  $curr_net[$nb_nodes]{"nb_params"}=0;
  $curr_net[$nb_nodes]{"nb_inputs"}=0;
  $curr_net[$nb_nodes]{"id"}=$curr_node_id;
  while (<>)
    {
      if (/(.*)\#<\/dia:string>/)
	{
	  if ($istype)
	    {
	      $curr_net[$nb_nodes]{"type"} = $1;
	      $istype=0;
	    } else {
	      /(.*)=(.*)\#<\/dia:string>/;
	      $curr_net[$nb_nodes]{"param"}[$param_count][0]=$1;
	      $curr_net[$nb_nodes]{"param"}[$param_count][1]=$2;
	      #print "   <param: $1, $2>\n";
	      $param_count++;
	      $curr_net[$nb_nodes]{"nb_params"}++;
	    }
	  return;	  
	} else {
	  chop;
	  if ($istype)
	    {
	      $curr_net[$nb_nodes]{"type"} = $1;
	      $istype=0;
	    } else {
	      /(.*)=(.*)/;
	      $curr_net[$nb_nodes]{"param"}[$param_count][0]=$1;
	      $curr_net[$nb_nodes]{"param"}[$param_count][1]=$2;
	      #print "   <param: $1, $2>\n";
	      $param_count++;
	      $curr_net[$nb_nodes]{"nb_params"}++;
	    }

	}
    }
}
