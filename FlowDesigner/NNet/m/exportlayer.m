function exportlayer(file, W,B,type)

ww=[W,B]';
w=ww(:);
[N,I] = size(W);
fprintf (file, "<FFLayer\n");
fprintf (file, "<nbNeurons %d>\n",N);
fprintf (file, "<nbInputs %d>\n",I);
fprintf (file, "<funcType %s >\n",type);
fprintf (file, "<weights ");
fprintf (file, "%f ",w);
fprintf (file, ">\n");
fprintf (file, ">\n");
