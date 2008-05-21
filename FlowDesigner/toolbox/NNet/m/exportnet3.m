function exportnet3 (filename,W1,B1,layer1,W2,B2,layer2,W3,B3,layer3)

[T2,T1]=size(W1);
[T3,T2]=size(W2);
[T4,T3]=size(W3);

file = fopen(filename,"w");

fprintf (file, "<FFNet\n");
fprintf (file, "<topo <Vector  %d %d %d %d > >\n",T1,T2,T3, T4);
fprintf (file, "<layers <Vector \n");

exportlayer(file, W1,B1,layer1);
exportlayer(file, W2,B2,layer2);
exportlayer(file, W3,B3,layer3);

fprintf (file, "> >\n");
fprintf (file, ">\n");
fclose(file);
