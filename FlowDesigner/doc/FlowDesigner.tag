<?xml version='1.0' encoding='ISO-8859-1' standalone='yes'?>
<tagfile>
  <compound kind="class">
    <name>AudioInfo</name>
    <filename>classAudioInfo.html</filename>
    <base>Object</base>
    <member kind="function">
      <type></type>
      <name>AudioInfo</name>
      <anchor>a0</anchor>
      <arglist>(string _ortho, int _length)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>AudioInfo</name>
      <anchor>a1</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>isCoarseEndpointed</name>
      <anchor>a2</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>isFineEndpointed</name>
      <anchor>a3</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>isWithinCoarse</name>
      <anchor>a4</anchor>
      <arglist>(int sample)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>isWithinFine</name>
      <anchor>a5</anchor>
      <arglist>(int sample)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>printOn</name>
      <anchor>a6</anchor>
      <arglist>(ostream &amp;out=cout) const </arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>readFrom</name>
      <anchor>a7</anchor>
      <arglist>(istream &amp;in=cin)</arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>string</type>
      <name>ortho</name>
      <anchor>p0</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>int</type>
      <name>length</name>
      <anchor>p1</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>bool</type>
      <name>coarse_endpointed</name>
      <anchor>p2</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>int</type>
      <name>coarse_start</name>
      <anchor>p3</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>int</type>
      <name>coarse_end</name>
      <anchor>p4</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>bool</type>
      <name>fine_endpointed</name>
      <anchor>p5</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>int</type>
      <name>fine_start</name>
      <anchor>p6</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>int</type>
      <name>fine_end</name>
      <anchor>p7</anchor>
      <arglist></arglist>
    </member>
    <member kind="friend">
      <type>friend istream &amp;</type>
      <name>operator&gt;&gt;</name>
      <anchor>n0</anchor>
      <arglist>(istream &amp;in, AudioInfo &amp;info)</arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>BaseException</name>
    <filename>classBaseException.html</filename>
    <member kind="function" virtualness="pure">
      <type>virtual void</type>
      <name>print</name>
      <anchor>a0</anchor>
      <arglist>(ostream &amp;out=cerr)=0</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>freeze</name>
      <anchor>a1</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual</type>
      <name>~BaseException</name>
      <anchor>a2</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual BaseException *</type>
      <name>add</name>
      <anchor>a3</anchor>
      <arglist>(BaseException *e)</arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>BaseVector</name>
    <filename>classBaseVector.html</filename>
    <base>Object</base>
    <member kind="function">
      <type></type>
      <name>BaseVector</name>
      <anchor>a0</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="pure">
      <type>virtual size_t</type>
      <name>vsize</name>
      <anchor>a1</anchor>
      <arglist>() const =0</arglist>
    </member>
    <member kind="function" virtualness="pure">
      <type>virtual bool</type>
      <name>vempty</name>
      <anchor>a2</anchor>
      <arglist>() const =0</arglist>
    </member>
    <member kind="function" virtualness="pure">
      <type>virtual ObjectRef</type>
      <name>range</name>
      <anchor>a3</anchor>
      <arglist>(size_t startInd, size_t endInd)=0</arglist>
    </member>
    <member kind="function" virtualness="pure">
      <type>virtual ObjectRef</type>
      <name>getIndex</name>
      <anchor>a4</anchor>
      <arglist>(int pos)=0</arglist>
    </member>
    <member kind="function" virtualness="pure">
      <type>virtual void</type>
      <name>setIndex</name>
      <anchor>a5</anchor>
      <arglist>(int pos, ObjectRef val)=0</arglist>
    </member>
    <member kind="function" virtualness="pure">
      <type>virtual ObjectRef</type>
      <name>clone</name>
      <anchor>a6</anchor>
      <arglist>()=0</arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>Buffer</name>
    <filename>classBuffer.html</filename>
    <base>Object</base>
    <member kind="function">
      <type></type>
      <name>Buffer</name>
      <anchor>a0</anchor>
      <arglist>(int bLength)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>Buffer</name>
      <anchor>a1</anchor>
      <arglist>(const Buffer &amp;)</arglist>
    </member>
    <member kind="function">
      <type>ObjectRef &amp;</type>
      <name>get</name>
      <anchor>a2</anchor>
      <arglist>(int ind) const </arglist>
    </member>
    <member kind="function">
      <type>ObjectRef &amp;</type>
      <name>operator[]</name>
      <anchor>a3</anchor>
      <arglist>(int ind)</arglist>
    </member>
    <member kind="function">
      <type>int</type>
      <name>isValid</name>
      <anchor>a4</anchor>
      <arglist>(int ind) const </arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>printOn</name>
      <anchor>a5</anchor>
      <arglist>(ostream &amp;out=cout) const </arglist>
    </member>
    <member kind="function">
      <type>int</type>
      <name>getCurrentPos</name>
      <anchor>a6</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>vector&lt; ObjectRef &gt;</type>
      <name>data</name>
      <anchor>p0</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>vector&lt; int &gt;</type>
      <name>flags</name>
      <anchor>p1</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>int</type>
      <name>bufferLength</name>
      <anchor>p2</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>int</type>
      <name>bufferPos</name>
      <anchor>p3</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>int</type>
      <name>currentPos</name>
      <anchor>p4</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>BufferedNode</name>
    <filename>classBufferedNode.html</filename>
    <base>Node</base>
    <member kind="function">
      <type></type>
      <name>BufferedNode</name>
      <anchor>a0</anchor>
      <arglist>(string nodeName, const ParameterSet &amp;params)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual</type>
      <name>~BufferedNode</name>
      <anchor>a1</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual ObjectRef</type>
      <name>getOutput</name>
      <anchor>a2</anchor>
      <arglist>(int output_id, int count)</arglist>
    </member>
    <member kind="function" virtualness="pure">
      <type>virtual void</type>
      <name>calculate</name>
      <anchor>a3</anchor>
      <arglist>(int output_id, int count, Buffer &amp;buf)=0</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>request</name>
      <anchor>a4</anchor>
      <arglist>(int outputID, const ParameterSet &amp;req)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>initialize</name>
      <anchor>a5</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>reset</name>
      <anchor>a6</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual int</type>
      <name>addOutput</name>
      <anchor>a7</anchor>
      <arglist>(const string &amp;outputName)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual int</type>
      <name>addInput</name>
      <anchor>a8</anchor>
      <arglist>(const string &amp;inputName)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>initializeBuffers</name>
      <anchor>a9</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>performRequests</name>
      <anchor>a10</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" protection="protected">
      <type></type>
      <name>BufferedNode</name>
      <anchor>b0</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>int</type>
      <name>processCount</name>
      <anchor>p0</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>vector&lt; OutputCacheInfo &gt;</type>
      <name>outputs</name>
      <anchor>p1</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>vector&lt; InputCacheInfo &gt;</type>
      <name>inputsCache</name>
      <anchor>p2</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>bool</type>
      <name>inOrder</name>
      <anchor>p3</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>CastException</name>
    <filename>classCastException.html</filename>
    <templarg>T</templarg>
    <member kind="function">
      <type></type>
      <name>CastException</name>
      <anchor>a0</anchor>
      <arglist>(string _type)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>print</name>
      <anchor>a1</anchor>
      <arglist>(ostream &amp;out=cerr)</arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>string</type>
      <name>type</name>
      <anchor>p0</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>Complex</name>
    <filename>classComplex.html</filename>
    <templarg>T</templarg>
    <base>Object</base>
    <member kind="typedef">
      <type>complex&lt; T &gt;</type>
      <name>basicType</name>
      <anchor>w0</anchor>
      <arglist></arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>Complex</name>
      <anchor>a0</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>Complex</name>
      <anchor>a1</anchor>
      <arglist>(const complex&lt; T &gt; &amp;val)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>Complex</name>
      <anchor>a2</anchor>
      <arglist>(const Complex&lt; T &gt; &amp;val)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>Complex</name>
      <anchor>a3</anchor>
      <arglist>(const NetCType&lt; complex&lt; T &gt; &gt; &amp;obj)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>printOn</name>
      <anchor>a4</anchor>
      <arglist>(ostream &amp;out) const </arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>readFrom</name>
      <anchor>a5</anchor>
      <arglist>(istream &amp;in)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>serialize</name>
      <anchor>a6</anchor>
      <arglist>(ostream &amp;out) const </arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>unserialize</name>
      <anchor>a7</anchor>
      <arglist>(istream &amp;in)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>prettyPrint</name>
      <anchor>a8</anchor>
      <arglist>(ostream &amp;out) const </arglist>
    </member>
    <member kind="function">
      <type>complex&lt; T &gt; &amp;</type>
      <name>val</name>
      <anchor>a9</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>destroy</name>
      <anchor>a10</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual ObjectRef</type>
      <name>clone</name>
      <anchor>a11</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" static="yes">
      <type>Complex&lt; T &gt; *</type>
      <name>alloc</name>
      <anchor>e0</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" static="yes">
      <type>Complex&lt; T &gt; *</type>
      <name>alloc</name>
      <anchor>e1</anchor>
      <arglist>(const Complex&lt; T &gt; &amp;obj)</arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>CompositeType</name>
    <filename>classCompositeType.html</filename>
    <base>Object</base>
    <member kind="function">
      <type>void</type>
      <name>printOn</name>
      <anchor>a1</anchor>
      <arglist>(ostream &amp;out) const </arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>readFrom</name>
      <anchor>a2</anchor>
      <arglist>(istream &amp;in)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>addField</name>
      <anchor>a3</anchor>
      <arglist>(const string &amp;name, ObjectRef obj)</arglist>
    </member>
    <member kind="function">
      <type>ObjectRef</type>
      <name>get</name>
      <anchor>a4</anchor>
      <arglist>(const string &amp;name) const </arglist>
    </member>
    <member kind="function">
      <type>map_type</type>
      <name>getAllFields</name>
      <anchor>a5</anchor>
      <arglist>()</arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>Constant</name>
    <filename>classConstant.html</filename>
    <base>Node</base>
    <member kind="function">
      <type></type>
      <name>Constant</name>
      <anchor>a0</anchor>
      <arglist>(string nodeName, ParameterSet params)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>initialize</name>
      <anchor>a1</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>reset</name>
      <anchor>a2</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual ObjectRef</type>
      <name>getOutput</name>
      <anchor>a3</anchor>
      <arglist>(int output_id, int count)</arglist>
    </member>
    <member kind="function" protection="protected">
      <type></type>
      <name>Constant</name>
      <anchor>b0</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>ObjectRef</type>
      <name>value</name>
      <anchor>p0</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>int</type>
      <name>outputID</name>
      <anchor>p1</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>Covariance</name>
    <filename>classCovariance.html</filename>
    <base>Object</base>
    <member kind="function">
      <type></type>
      <name>Covariance</name>
      <anchor>a0</anchor>
      <arglist>(int dim)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>Covariance</name>
      <anchor>a1</anchor>
      <arglist>(const Covariance &amp;cov)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual</type>
      <name>~Covariance</name>
      <anchor>a2</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="pure">
      <type>virtual void</type>
      <name>accumFrame</name>
      <anchor>a3</anchor>
      <arglist>(const float *v)=0</arglist>
    </member>
    <member kind="function" virtualness="pure">
      <type>virtual void</type>
      <name>accumFrame</name>
      <anchor>a4</anchor>
      <arglist>(const vector&lt; float &gt; &amp;v)=0</arglist>
    </member>
    <member kind="function">
      <type>unsigned int</type>
      <name>size</name>
      <anchor>a5</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function">
      <type>double</type>
      <name>getDeterminant</name>
      <anchor>a6</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function" virtualness="pure">
      <type>virtual void</type>
      <name>compute_determinant</name>
      <anchor>a7</anchor>
      <arglist>() const =0</arglist>
    </member>
    <member kind="function" virtualness="pure">
      <type>virtual void</type>
      <name>printOn</name>
      <anchor>a8</anchor>
      <arglist>(ostream &amp;out=cout) const =0</arglist>
    </member>
    <member kind="function" virtualness="pure">
      <type>virtual double</type>
      <name>mahalanobisDistance</name>
      <anchor>a9</anchor>
      <arglist>(const float *x1, const double *x2) const =0</arglist>
    </member>
    <member kind="function" virtualness="pure">
      <type>virtual double &amp;</type>
      <name>operator[]</name>
      <anchor>a10</anchor>
      <arglist>(int)=0</arglist>
    </member>
    <member kind="function" virtualness="pure">
      <type>virtual double &amp;</type>
      <name>operator()</name>
      <anchor>a11</anchor>
      <arglist>(int, int)=0</arglist>
    </member>
    <member kind="function" virtualness="pure">
      <type>virtual void</type>
      <name>reset</name>
      <anchor>a12</anchor>
      <arglist>()=0</arglist>
    </member>
    <member kind="function" virtualness="pure">
      <type>virtual Covariance *</type>
      <name>copy</name>
      <anchor>a13</anchor>
      <arglist>()=0</arglist>
    </member>
    <member kind="function" virtualness="pure">
      <type>virtual void</type>
      <name>invert</name>
      <anchor>a14</anchor>
      <arglist>()=0</arglist>
    </member>
    <member kind="function" virtualness="pure">
      <type>virtual void</type>
      <name>processMean</name>
      <anchor>a15</anchor>
      <arglist>(RCPtr&lt; Mean &gt; mean)=0</arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>int</type>
      <name>dimension</name>
      <anchor>p0</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>double</type>
      <name>determinant</name>
      <anchor>p1</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>bool</type>
      <name>determinant_is_valid</name>
      <anchor>p2</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>int</type>
      <name>mode</name>
      <anchor>p3</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>int</type>
      <name>accum_count</name>
      <anchor>p4</anchor>
      <arglist></arglist>
    </member>
    <member kind="friend">
      <type>friend class</type>
      <name>GMM</name>
      <anchor>n0</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>DiagGMM</name>
    <filename>classDiagGMM.html</filename>
    <base>Object</base>
    <member kind="function">
      <type></type>
      <name>~DiagGMM</name>
      <anchor>a1</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>train</name>
      <anchor>a2</anchor>
      <arglist>(const vector&lt; float * &gt; &amp;frames, int nb_dim, int nb_gaussians, int nb_splits)</arglist>
    </member>
    <member kind="function">
      <type>float</type>
      <name>score</name>
      <anchor>a3</anchor>
      <arglist>(const float *vec)</arglist>
    </member>
    <member kind="function">
      <type>int</type>
      <name>getDim</name>
      <anchor>a4</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>printOn</name>
      <anchor>a5</anchor>
      <arglist>(ostream &amp;out=cout) const </arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>readFrom</name>
      <anchor>a6</anchor>
      <arglist>(istream &amp;in=cin)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>serialize</name>
      <anchor>a7</anchor>
      <arglist>(ostream &amp;out) const </arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>unserialize</name>
      <anchor>a8</anchor>
      <arglist>(istream &amp;in)</arglist>
    </member>
    <member kind="friend">
      <type>friend class</type>
      <name>GMM</name>
      <anchor>n0</anchor>
      <arglist></arglist>
    </member>
    <member kind="friend">
      <type>friend istream &amp;</type>
      <name>operator&gt;&gt;</name>
      <anchor>n1</anchor>
      <arglist>(istream &amp;in, DiagGMM &amp;gmm)</arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>DiagonalCovariance</name>
    <filename>classDiagonalCovariance.html</filename>
    <base>Covariance</base>
    <member kind="function">
      <type></type>
      <name>DiagonalCovariance</name>
      <anchor>a0</anchor>
      <arglist>(istream &amp;in)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>DiagonalCovariance</name>
      <anchor>a2</anchor>
      <arglist>(int dim)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>DiagonalCovariance</name>
      <anchor>a3</anchor>
      <arglist>(const DiagonalCovariance &amp;cov)</arglist>
    </member>
    <member kind="function">
      <type>double</type>
      <name>mahalanobisDistance</name>
      <anchor>a4</anchor>
      <arglist>(const float *x1, const double *x2) const </arglist>
    </member>
    <member kind="function">
      <type>double &amp;</type>
      <name>operator[]</name>
      <anchor>a5</anchor>
      <arglist>(int i)</arglist>
    </member>
    <member kind="function">
      <type>double &amp;</type>
      <name>operator()</name>
      <anchor>a6</anchor>
      <arglist>(int i)</arglist>
    </member>
    <member kind="function">
      <type>double &amp;</type>
      <name>operator()</name>
      <anchor>a7</anchor>
      <arglist>(int i, int)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>compute_determinant</name>
      <anchor>a8</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>accumFrame</name>
      <anchor>a9</anchor>
      <arglist>(const float *v)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>accumFrame</name>
      <anchor>a10</anchor>
      <arglist>(const vector&lt; float &gt; &amp;v)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>reset</name>
      <anchor>a11</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>Covariance *</type>
      <name>copy</name>
      <anchor>a12</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>invert</name>
      <anchor>a13</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>processMean</name>
      <anchor>a14</anchor>
      <arglist>(RCPtr&lt; Mean &gt; mean)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>printOn</name>
      <anchor>a15</anchor>
      <arglist>(ostream &amp;out=cout) const </arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>readFrom</name>
      <anchor>a16</anchor>
      <arglist>(istream &amp;in=cin)</arglist>
    </member>
    <member kind="friend">
      <type>friend class</type>
      <name>GMM</name>
      <anchor>n0</anchor>
      <arglist></arglist>
    </member>
    <member kind="friend">
      <type>friend istream &amp;</type>
      <name>operator&gt;&gt;</name>
      <anchor>n1</anchor>
      <arglist>(istream &amp;in, DiagonalCovariance &amp;cov)</arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>DLManager</name>
    <filename>classDLManager.html</filename>
    <member kind="function" static="yes">
      <type>LoadedLibrary *</type>
      <name>getLib</name>
      <anchor>e0</anchor>
      <arglist>(const string &amp;name)</arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>FactoryNotFoundException</name>
    <filename>classFactoryNotFoundException.html</filename>
    <base>BaseException</base>
    <member kind="function">
      <type></type>
      <name>FactoryNotFoundException</name>
      <anchor>a0</anchor>
      <arglist>(string name)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>print</name>
      <anchor>a1</anchor>
      <arglist>(ostream &amp;out=cerr)</arglist>
    </member>
    <member kind="variable">
      <type>string</type>
      <name>factoryName</name>
      <anchor>o0</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>FFLayer</name>
    <filename>classFFLayer.html</filename>
    <base>Object</base>
    <member kind="function">
      <type></type>
      <name>FFLayer</name>
      <anchor>a0</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>FFLayer</name>
      <anchor>a1</anchor>
      <arglist>(int _nbNeurons, int _nbInputs, float *_weights, int _weightOffset, int _neuronOffset, string type=&quot;tansig&quot;)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>FFLayer</name>
      <anchor>a2</anchor>
      <arglist>(const FFLayer &amp;layer)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>setupAfterRead</name>
      <anchor>a3</anchor>
      <arglist>(float *_weights, int _weightOffset, int _neuronOffset)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>update</name>
      <anchor>a5</anchor>
      <arglist>(const float *previous, float *value, float *deriv=NULL)</arglist>
    </member>
    <member kind="function">
      <type>int</type>
      <name>size</name>
      <anchor>a6</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>int</type>
      <name>getNbWeights</name>
      <anchor>a7</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>int</type>
      <name>getNeuronWeightOffset</name>
      <anchor>a8</anchor>
      <arglist>(int i)</arglist>
    </member>
    <member kind="function">
      <type>int</type>
      <name>getWeightOffset</name>
      <anchor>a9</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>int</type>
      <name>getNeuronOffset</name>
      <anchor>a10</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>init</name>
      <anchor>a11</anchor>
      <arglist>(float minmax)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>init</name>
      <anchor>a12</anchor>
      <arglist>(double *mean, double *std)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>setBias</name>
      <anchor>a13</anchor>
      <arglist>(double *minmax)</arglist>
    </member>
    <member kind="function">
      <type>float *</type>
      <name>getWeights</name>
      <anchor>a14</anchor>
      <arglist>(int i)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>printOn</name>
      <anchor>a15</anchor>
      <arglist>(ostream &amp;out) const </arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>readFrom</name>
      <anchor>a16</anchor>
      <arglist>(istream &amp;in)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>setDerivOffset</name>
      <anchor>a17</anchor>
      <arglist>(float d)</arglist>
    </member>
    <member kind="variable">
      <type>void(*</type>
      <name>func</name>
      <anchor>o0</anchor>
      <arglist>)(float *, float *, int)</arglist>
    </member>
    <member kind="variable">
      <type>void(*</type>
      <name>deriv_func</name>
      <anchor>o1</anchor>
      <arglist>)(float *, float *, int)</arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>int</type>
      <name>nbNeurons</name>
      <anchor>p0</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>int</type>
      <name>nbInputs</name>
      <anchor>p1</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>float *</type>
      <name>weights</name>
      <anchor>p2</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>string</type>
      <name>funcType</name>
      <anchor>p3</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>int</type>
      <name>weightOffset</name>
      <anchor>p4</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>int</type>
      <name>neuronOffset</name>
      <anchor>p5</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>float</type>
      <name>derivOffset</name>
      <anchor>p6</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>FFNet</name>
    <filename>classFFNet.html</filename>
    <base>Object</base>
    <member kind="function">
      <type></type>
      <name>FFNet</name>
      <anchor>a0</anchor>
      <arglist>(const Vector&lt; int &gt; &amp;_topo, const Vector&lt; string &gt; &amp;functions)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>FFNet</name>
      <anchor>a2</anchor>
      <arglist>(FFNet &amp;net)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>FFNet</name>
      <anchor>a3</anchor>
      <arglist>(const Vector&lt; int &gt; &amp;_topo, const Vector&lt; string &gt; &amp;functions, vector&lt; float * &gt; &amp;tin, vector&lt; float * &gt; &amp;tout)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>init</name>
      <anchor>a4</anchor>
      <arglist>(const Vector&lt; string &gt; &amp;functions)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>setupLayersAfterRead</name>
      <anchor>a5</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>float *</type>
      <name>calc</name>
      <anchor>a6</anchor>
      <arglist>(const float *input, float *value, float *deriv=NULL)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>learn</name>
      <anchor>a7</anchor>
      <arglist>(float *input, float *output, double *gradient, double *err=NULL, float *calc_output=NULL)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>calcGradient</name>
      <anchor>a8</anchor>
      <arglist>(vector&lt; float * &gt; &amp;tin, vector&lt; float * &gt; &amp;tout, Array&lt; float &gt; weights, Array&lt; double &gt; &amp;gradient, double &amp;err)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>weightedLearn</name>
      <anchor>a9</anchor>
      <arglist>(float *input, float *output, float *learnWeights, double *gradient, double *err=NULL, float *calc_output=NULL)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>weightedCalcGradient</name>
      <anchor>a10</anchor>
      <arglist>(vector&lt; float * &gt; &amp;tin, vector&lt; float * &gt; &amp;tout, vector&lt; float * &gt; &amp;learnWeights, Array&lt; float &gt; weights, Array&lt; double &gt; &amp;gradient, double &amp;err)</arglist>
    </member>
    <member kind="function">
      <type>float</type>
      <name>totalError</name>
      <anchor>a11</anchor>
      <arglist>(vector&lt; float * &gt; tin, vector&lt; float * &gt; tout)</arglist>
    </member>
    <member kind="function">
      <type>int</type>
      <name>getNbWeights</name>
      <anchor>a12</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>int</type>
      <name>getNbNeurons</name>
      <anchor>a13</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>const Vector&lt; int &gt; &amp;</type>
      <name>getTopo</name>
      <anchor>a14</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>const Vector&lt; RCPtr&lt; FFLayer &gt; &gt; &amp;</type>
      <name>getLayers</name>
      <anchor>a15</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>const float *</type>
      <name>getWeights</name>
      <anchor>a16</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>setWeights</name>
      <anchor>a17</anchor>
      <arglist>(float *ptr)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>setDerivOffset</name>
      <anchor>a18</anchor>
      <arglist>(float d)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>printOn</name>
      <anchor>a19</anchor>
      <arglist>(ostream &amp;out) const </arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>readFrom</name>
      <anchor>a20</anchor>
      <arglist>(istream &amp;in)</arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>Vector&lt; int &gt;</type>
      <name>topo</name>
      <anchor>p0</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>Vector&lt; RCPtr&lt; FFLayer &gt; &gt;</type>
      <name>layers</name>
      <anchor>p1</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>float *</type>
      <name>weights</name>
      <anchor>p2</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>int</type>
      <name>nbNeurons</name>
      <anchor>p3</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>int</type>
      <name>nbWeights</name>
      <anchor>p4</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>FILEDES</name>
    <filename>classFILEDES.html</filename>
    <base>GenericType&lt; int &gt;</base>
    <member kind="function">
      <type></type>
      <name>FILEDES</name>
      <anchor>a0</anchor>
      <arglist>(int fd)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>FILEDES</name>
      <anchor>a1</anchor>
      <arglist>(const string &amp;filename, int mode)</arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>FILEPTR</name>
    <filename>classFILEPTR.html</filename>
    <base>GenericType&lt; FILE * &gt;</base>
    <member kind="function">
      <type></type>
      <name>FILEPTR</name>
      <anchor>a0</anchor>
      <arglist>(FILE *file)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>FILEPTR</name>
      <anchor>a1</anchor>
      <arglist>(const string &amp;filename, const string &amp;mode)</arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>Gaussian</name>
    <filename>classGaussian.html</filename>
    <base>Object</base>
    <member kind="function">
      <type></type>
      <name>Gaussian</name>
      <anchor>a0</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>Gaussian</name>
      <anchor>a1</anchor>
      <arglist>(istream &amp;in)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>Gaussian</name>
      <anchor>a2</anchor>
      <arglist>(int dim, Covariance *(*cov_new)(int))</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>Gaussian</name>
      <anchor>a3</anchor>
      <arglist>(int dim, int _meanID, int _covarianceID)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>Gaussian</name>
      <anchor>a4</anchor>
      <arglist>(const Gaussian &amp;g)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>~Gaussian</name>
      <anchor>a5</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>int</type>
      <name>getDimension</name>
      <anchor>a6</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function">
      <type>Mean &amp;</type>
      <name>getMean</name>
      <anchor>a7</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function">
      <type>Covariance &amp;</type>
      <name>getCovariance</name>
      <anchor>a8</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>to_real</name>
      <anchor>a9</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>int</type>
      <name>get_accum_count</name>
      <anchor>a10</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function">
      <type>double</type>
      <name>mahalanobis</name>
      <anchor>a11</anchor>
      <arglist>(const float *fr) const </arglist>
    </member>
    <member kind="function">
      <type>double</type>
      <name>mahalanobis</name>
      <anchor>a12</anchor>
      <arglist>(const float *fr, Covariance *cov) const </arglist>
    </member>
    <member kind="function">
      <type>double</type>
      <name>euclidian</name>
      <anchor>a13</anchor>
      <arglist>(const float *fr) const </arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>accum_frame</name>
      <anchor>a14</anchor>
      <arglist>(const float *fr)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>reset_to_accum_mode</name>
      <anchor>a15</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>toIDsUsing</name>
      <anchor>a16</anchor>
      <arglist>(MeanSet &amp;means, CovarianceSet &amp;covariances)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>toPtrsUsing</name>
      <anchor>a17</anchor>
      <arglist>(const MeanSet &amp;means, const CovarianceSet &amp;covariances)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>printOn</name>
      <anchor>a18</anchor>
      <arglist>(ostream &amp;out=cout) const </arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>readFrom</name>
      <anchor>a19</anchor>
      <arglist>(istream &amp;in=cin)</arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>RCPtr&lt; Mean &gt;</type>
      <name>mean</name>
      <anchor>p0</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>RCPtr&lt; Covariance &gt;</type>
      <name>covariance</name>
      <anchor>p1</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>int</type>
      <name>accum_count</name>
      <anchor>p2</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>int</type>
      <name>dimension</name>
      <anchor>p3</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>bool</type>
      <name>using_meanID</name>
      <anchor>p4</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>bool</type>
      <name>using_covarianceID</name>
      <anchor>p5</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>int</type>
      <name>meanID</name>
      <anchor>p6</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>int</type>
      <name>covarianceID</name>
      <anchor>p7</anchor>
      <arglist></arglist>
    </member>
    <member kind="friend">
      <type>friend class</type>
      <name>GMM</name>
      <anchor>n0</anchor>
      <arglist></arglist>
    </member>
    <member kind="friend">
      <type>friend istream &amp;</type>
      <name>operator&gt;&gt;</name>
      <anchor>n1</anchor>
      <arglist>(istream &amp;in, Gaussian &amp;gauss)</arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>GeneralException</name>
    <filename>classGeneralException.html</filename>
    <base>BaseException</base>
    <member kind="function">
      <type></type>
      <name>GeneralException</name>
      <anchor>a0</anchor>
      <arglist>(string _message, string _file, int _line)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>print</name>
      <anchor>a1</anchor>
      <arglist>(ostream &amp;out=cerr)</arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>string</type>
      <name>message</name>
      <anchor>p0</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>string</type>
      <name>file</name>
      <anchor>p1</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>int</type>
      <name>line</name>
      <anchor>p2</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>GenericType</name>
    <filename>classGenericType.html</filename>
    <templarg>T</templarg>
    <base>Object</base>
    <member kind="typedef">
      <type>T</type>
      <name>basicType</name>
      <anchor>w0</anchor>
      <arglist></arglist>
    </member>
    <member kind="function">
      <type>T &amp;</type>
      <name>val</name>
      <anchor>a0</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>GenericType</name>
      <anchor>a1</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual</type>
      <name>~GenericType</name>
      <anchor>a2</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>GenericType</name>
      <anchor>a3</anchor>
      <arglist>(T val)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>GenericType</name>
      <anchor>a4</anchor>
      <arglist>(GenericType&lt; T &gt; &amp;copy)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>printOn</name>
      <anchor>a5</anchor>
      <arglist>(ostream &amp;out=cout) const </arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>T</type>
      <name>value</name>
      <anchor>p0</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>GMM</name>
    <filename>classGMM.html</filename>
    <base>Object</base>
    <member kind="function">
      <type></type>
      <name>GMM</name>
      <anchor>a0</anchor>
      <arglist>(int nb_gauss, int dim, Covariance *(*cov_new)(int))</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>nb_gaussians</name>
      <anchor>a1</anchor>
      <arglist>(0)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>mode</name>
      <anchor>a2</anchor>
      <arglist>(accum)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>nb_frames_aligned</name>
      <anchor>a3</anchor>
      <arglist>(0)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>dimensions</name>
      <anchor>a4</anchor>
      <arglist>(1)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>using_gaussianIDs</name>
      <anchor>a5</anchor>
      <arglist>(false)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>save</name>
      <anchor>a6</anchor>
      <arglist>(string file)</arglist>
    </member>
    <member kind="function">
      <type>int</type>
      <name>get_nb_gaussians</name>
      <anchor>a7</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function">
      <type>Gaussian &amp;</type>
      <name>gaussian</name>
      <anchor>a8</anchor>
      <arglist>(int i) const </arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>accum_to_gaussian</name>
      <anchor>a9</anchor>
      <arglist>(int i, const float *fr)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>init</name>
      <anchor>a10</anchor>
      <arglist>(vector&lt; float * &gt; frames)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>kmeans1</name>
      <anchor>a11</anchor>
      <arglist>(vector&lt; float * &gt; frames, int nb_iterations=1)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>split1</name>
      <anchor>a12</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>kmeans2</name>
      <anchor>a13</anchor>
      <arglist>(vector&lt; float * &gt; frames, GMM *gmm)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>adaptMAP</name>
      <anchor>a14</anchor>
      <arglist>(vector&lt; float * &gt; frame, GMM *gmm)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>to_real</name>
      <anchor>a15</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>reset_to_accum_mode</name>
      <anchor>a16</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>Score</type>
      <name>minDistance</name>
      <anchor>a17</anchor>
      <arglist>(float *fr, Covariance *cov) const </arglist>
    </member>
    <member kind="function">
      <type>Score</type>
      <name>score</name>
      <anchor>a18</anchor>
      <arglist>(float *fr) const </arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>binary_split</name>
      <anchor>a19</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>vector&lt; Score &gt;</type>
      <name>minDistance</name>
      <anchor>a20</anchor>
      <arglist>(vector&lt; float * &gt; fr) const </arglist>
    </member>
    <member kind="function">
      <type>vector&lt; Score &gt;</type>
      <name>score</name>
      <anchor>a21</anchor>
      <arglist>(vector&lt; float * &gt; fr) const </arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>toIDsUsing</name>
      <anchor>a22</anchor>
      <arglist>(GaussianSet &amp;gauss)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>toPtrsUsing</name>
      <anchor>a23</anchor>
      <arglist>(const GaussianSet &amp;gauss)</arglist>
    </member>
    <member kind="function">
      <type>DiagGMM *</type>
      <name>createDiagGMM</name>
      <anchor>a24</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>printOn</name>
      <anchor>a25</anchor>
      <arglist>(ostream &amp;out=cout) const </arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>readFrom</name>
      <anchor>a26</anchor>
      <arglist>(istream &amp;in=cin)</arglist>
    </member>
    <member kind="variable">
      <type></type>
      <name>__pad0__</name>
      <anchor>o0</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>vector&lt; RCPtr&lt; Gaussian &gt; &gt;</type>
      <name>gaussians</name>
      <anchor>p0</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>vector&lt; float &gt;</type>
      <name>apriori</name>
      <anchor>p1</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>int</type>
      <name>nb_gaussians</name>
      <anchor>p2</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>int</type>
      <name>mode</name>
      <anchor>p3</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>int</type>
      <name>nb_frames_aligned</name>
      <anchor>p4</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>int</type>
      <name>dimensions</name>
      <anchor>p5</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>bool</type>
      <name>using_gaussianIDs</name>
      <anchor>p6</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>vector&lt; int &gt;</type>
      <name>gaussianIDs</name>
      <anchor>p7</anchor>
      <arglist></arglist>
    </member>
    <member kind="friend">
      <type>friend istream &amp;</type>
      <name>operator&gt;&gt;</name>
      <anchor>n0</anchor>
      <arglist>(istream &amp;in, GMM &amp;gmm)</arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>InputTranslator</name>
    <filename>classInputTranslator.html</filename>
    <base>BufferedNode</base>
    <member kind="function">
      <type></type>
      <name>InputTranslator</name>
      <anchor>a0</anchor>
      <arglist>(string nodeName, ParameterSet params)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>setProcessCount</name>
      <anchor>a1</anchor>
      <arglist>(int pc)</arglist>
    </member>
    <member kind="function">
      <type>int</type>
      <name>getProcessCount</name>
      <anchor>a2</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>request</name>
      <anchor>a3</anchor>
      <arglist>(int outputID, const ParameterSet &amp;req)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual ObjectRef</type>
      <name>getOutput</name>
      <anchor>a4</anchor>
      <arglist>(int output_id, int count)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>calculate</name>
      <anchor>a5</anchor>
      <arglist>(int output_id, int count, Buffer &amp;out)</arglist>
    </member>
    <member kind="function">
      <type>int</type>
      <name>addInput</name>
      <anchor>a6</anchor>
      <arglist>(const string &amp;inputName)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual int</type>
      <name>translateInput</name>
      <anchor>a7</anchor>
      <arglist>(string inputName)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual int</type>
      <name>translateOutput</name>
      <anchor>a8</anchor>
      <arglist>(string outputName)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual bool</type>
      <name>hasOutput</name>
      <anchor>a9</anchor>
      <arglist>(int output_id) const </arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>requestForIterator</name>
      <anchor>a10</anchor>
      <arglist>(const ParameterSet &amp;req)</arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>int</type>
      <name>internal_processCount</name>
      <anchor>p0</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>IOStream</name>
    <filename>classIOStream.html</filename>
    <base>IStream</base>
    <base>OStream</base>
    <member kind="function">
      <type></type>
      <name>IOStream</name>
      <anchor>a0</anchor>
      <arglist>(iostream *_str, bool _owner=true)</arglist>
    </member>
    <member kind="function">
      <type>int</type>
      <name>eof</name>
      <anchor>a1</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>int</type>
      <name>fail</name>
      <anchor>a2</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>operator iostream &amp;</name>
      <anchor>a3</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>printOn</name>
      <anchor>a4</anchor>
      <arglist>(ostream &amp;out) const </arglist>
    </member>
    <member kind="function">
      <type>IOStream &amp;</type>
      <name>seekg</name>
      <anchor>a6</anchor>
      <arglist>(int pos, ios::seekdir dir)</arglist>
    </member>
    <member kind="function">
      <type>IOStream &amp;</type>
      <name>seekp</name>
      <anchor>a7</anchor>
      <arglist>(int pos, ios::seekdir dir)</arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>IStream</name>
    <filename>classIStream.html</filename>
    <base virtualness="virtual">Stream</base>
    <member kind="function">
      <type></type>
      <name>IStream</name>
      <anchor>a0</anchor>
      <arglist>(istream *_str, bool _owner=true)</arglist>
    </member>
    <member kind="function">
      <type>IStream &amp;</type>
      <name>read</name>
      <anchor>a1</anchor>
      <arglist>(char *ch, int len)</arglist>
    </member>
    <member kind="function">
      <type>int</type>
      <name>gcount</name>
      <anchor>a2</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>int</type>
      <name>eof</name>
      <anchor>a3</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>int</type>
      <name>fail</name>
      <anchor>a4</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>IStream &amp;</type>
      <name>getline</name>
      <anchor>a5</anchor>
      <arglist>(char *ch, int len)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>operator istream &amp;</name>
      <anchor>a6</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>printOn</name>
      <anchor>a7</anchor>
      <arglist>(ostream &amp;out) const </arglist>
    </member>
    <member kind="function">
      <type>IStream &amp;</type>
      <name>seekg</name>
      <anchor>a9</anchor>
      <arglist>(int pos, ios::seekdir dir)</arglist>
    </member>
    <member kind="function">
      <type>IStream &amp;</type>
      <name>operator&gt;&gt;</name>
      <anchor>a10</anchor>
      <arglist>(T &amp;obj)</arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>Iterator</name>
    <filename>classIterator.html</filename>
    <base>Network</base>
    <member kind="function">
      <type></type>
      <name>Iterator</name>
      <anchor>a0</anchor>
      <arglist>(string nodeName, ParameterSet params)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual ObjectRef</type>
      <name>getOutput</name>
      <anchor>a1</anchor>
      <arglist>(int output_id, int count)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>connectToNode</name>
      <anchor>a2</anchor>
      <arglist>(string in, Node *inNode, string out)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>request</name>
      <anchor>a3</anchor>
      <arglist>(int outputID, const ParameterSet &amp;req)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>setConditionNode</name>
      <anchor>a4</anchor>
      <arglist>(Node *aNode)</arglist>
    </member>
    <member kind="function">
      <type>Node *</type>
      <name>getConditionNode</name>
      <anchor>a5</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>initialize</name>
      <anchor>a6</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>reset</name>
      <anchor>a7</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>stop</name>
      <anchor>a8</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" protection="protected" virtualness="virtual">
      <type>virtual void</type>
      <name>connectToNode</name>
      <anchor>b0</anchor>
      <arglist>(unsigned int in, Node *inNode, unsigned int out)</arglist>
    </member>
    <member kind="function" protection="protected">
      <type></type>
      <name>Iterator</name>
      <anchor>b1</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>int</type>
      <name>processCount</name>
      <anchor>p0</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>bool</type>
      <name>doWhile</name>
      <anchor>p1</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>bool</type>
      <name>exit_status</name>
      <anchor>p2</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>Node *</type>
      <name>conditionNode</name>
      <anchor>p3</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>InputTranslator *</type>
      <name>translator</name>
      <anchor>p4</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>vector&lt; ObjectRef &gt;</type>
      <name>output</name>
      <anchor>p5</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>LoadedLibrary</name>
    <filename>classLoadedLibrary.html</filename>
    <member kind="function">
      <type></type>
      <name>LoadedLibrary</name>
      <anchor>a0</anchor>
      <arglist>(const string &amp;path)</arglist>
    </member>
    <member kind="function">
      <type>void *</type>
      <name>get_proc</name>
      <anchor>a1</anchor>
      <arglist>(string symbol)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>~LoadedLibrary</name>
      <anchor>a2</anchor>
      <arglist>()</arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>Matrix</name>
    <filename>classMatrix.html</filename>
    <templarg>T</templarg>
    <member kind="typedef">
      <type>T</type>
      <name>basicType</name>
      <anchor>w0</anchor>
      <arglist></arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>Matrix</name>
      <anchor>a0</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>Matrix</name>
      <anchor>a1</anchor>
      <arglist>(const Matrix &amp;mat, bool transpose=false)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>Matrix</name>
      <anchor>a2</anchor>
      <arglist>(int _rows, int _cols)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual</type>
      <name>~Matrix</name>
      <anchor>a3</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>resize</name>
      <anchor>a4</anchor>
      <arglist>(int _rows, int _cols)</arglist>
    </member>
    <member kind="function">
      <type>T *</type>
      <name>operator[]</name>
      <anchor>a5</anchor>
      <arglist>(int i)</arglist>
    </member>
    <member kind="function">
      <type>const T *</type>
      <name>operator[]</name>
      <anchor>a6</anchor>
      <arglist>(int i) const </arglist>
    </member>
    <member kind="function">
      <type>T &amp;</type>
      <name>operator()</name>
      <anchor>a7</anchor>
      <arglist>(int i, int j)</arglist>
    </member>
    <member kind="function">
      <type>const T &amp;</type>
      <name>operator()</name>
      <anchor>a8</anchor>
      <arglist>(int i, int j) const </arglist>
    </member>
    <member kind="function">
      <type>int</type>
      <name>nrows</name>
      <anchor>a9</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function">
      <type>int</type>
      <name>ncols</name>
      <anchor>a10</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>transpose</name>
      <anchor>a11</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>printOn</name>
      <anchor>a12</anchor>
      <arglist>(ostream &amp;out) const </arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>readFrom</name>
      <anchor>a13</anchor>
      <arglist>(istream &amp;in=cin)</arglist>
    </member>
    <member kind="function">
      <type>int</type>
      <name>size</name>
      <anchor>a14</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual size_t</type>
      <name>msize</name>
      <anchor>a15</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual bool</type>
      <name>mempty</name>
      <anchor>a16</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>serialize</name>
      <anchor>a17</anchor>
      <arglist>(ostream &amp;out) const </arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>unserialize</name>
      <anchor>a18</anchor>
      <arglist>(istream &amp;in)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual ObjectRef</type>
      <name>getIndex</name>
      <anchor>a19</anchor>
      <arglist>(int _row, int _col)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>setIndex</name>
      <anchor>a20</anchor>
      <arglist>(int _row, int _col, ObjectRef val)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual ObjectRef</type>
      <name>clone</name>
      <anchor>a21</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>ObjectRef</type>
      <name>clone</name>
      <anchor>a22</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" static="yes">
      <type>string</type>
      <name>GetClassName</name>
      <anchor>e0</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>int</type>
      <name>rows</name>
      <anchor>p0</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>int</type>
      <name>cols</name>
      <anchor>p1</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>T *</type>
      <name>data</name>
      <anchor>p2</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>Model</name>
    <filename>classModel.html</filename>
  </compound>
  <compound kind="class">
    <name>MSVQ</name>
    <filename>classMSVQ.html</filename>
    <base>VQ</base>
    <member kind="function">
      <type></type>
      <name>MSVQ</name>
      <anchor>a0</anchor>
      <arglist>(float(*_dist)(const float *, const float *, int)=KMeans::euclidian)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>MSVQ</name>
      <anchor>a1</anchor>
      <arglist>(const vector&lt; int &gt; &amp;_stagesSizes, float(*_dist)(const float *, const float *, int)=KMeans::euclidian)</arglist>
    </member>
    <member kind="function">
      <type>int</type>
      <name>ID2Vec</name>
      <anchor>a2</anchor>
      <arglist>(const vector&lt; int &gt; &amp;vec) const </arglist>
    </member>
    <member kind="function">
      <type>vector&lt; int &gt;</type>
      <name>Vec2ID</name>
      <anchor>a3</anchor>
      <arglist>(int ID) const </arglist>
    </member>
    <member kind="function">
      <type>int</type>
      <name>nbClasses</name>
      <anchor>a4</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function">
      <type>int</type>
      <name>nbStages</name>
      <anchor>a5</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>train</name>
      <anchor>a6</anchor>
      <arglist>(const vector&lt; float * &gt; &amp;data, int len, bool binary=false)</arglist>
    </member>
    <member kind="function">
      <type>int</type>
      <name>getClassID</name>
      <anchor>a7</anchor>
      <arglist>(const float *v, float *dist_return=NULL) const </arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>printOn</name>
      <anchor>a8</anchor>
      <arglist>(ostream &amp;out=cout) const </arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>readFrom</name>
      <anchor>a9</anchor>
      <arglist>(istream &amp;in=cin)</arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>vector&lt; int &gt;</type>
      <name>stagesSizes</name>
      <anchor>p0</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>vector&lt; KMeans &gt;</type>
      <name>stages</name>
      <anchor>p1</anchor>
      <arglist></arglist>
    </member>
    <member kind="friend">
      <type>friend istream &amp;</type>
      <name>operator&gt;&gt;</name>
      <anchor>n0</anchor>
      <arglist>(istream &amp;in, MSVQ &amp;mdl)</arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>NetCType</name>
    <filename>classNetCType.html</filename>
    <templarg>T</templarg>
    <base>PrintableGenericType</base>
    <member kind="function">
      <type></type>
      <name>NetCType</name>
      <anchor>a0</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>NetCType</name>
      <anchor>a1</anchor>
      <arglist>(T val)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual</type>
      <name>~NetCType</name>
      <anchor>a2</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>operator T</name>
      <anchor>a3</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>NetCType&lt; T &gt; &amp;</type>
      <name>operator=</name>
      <anchor>a4</anchor>
      <arglist>(NetCType&lt; T &gt; &amp;type)</arglist>
    </member>
    <member kind="function">
      <type>NetCType&lt; T &gt; &amp;</type>
      <name>operator=</name>
      <anchor>a5</anchor>
      <arglist>(T val)</arglist>
    </member>
    <member kind="function">
      <type>int</type>
      <name>operator==</name>
      <anchor>a6</anchor>
      <arglist>(NetCType&lt; T &gt; &amp;type)</arglist>
    </member>
    <member kind="function">
      <type>int</type>
      <name>operator==</name>
      <anchor>a7</anchor>
      <arglist>(T val)</arglist>
    </member>
    <member kind="function">
      <type>int</type>
      <name>operator!=</name>
      <anchor>a8</anchor>
      <arglist>(NetCType&lt; T &gt; &amp;type)</arglist>
    </member>
    <member kind="function">
      <type>int</type>
      <name>operator!=</name>
      <anchor>a9</anchor>
      <arglist>(T val)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>destroy</name>
      <anchor>a10</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" static="yes">
      <type>NetCType&lt; T &gt; *</type>
      <name>alloc</name>
      <anchor>e0</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" static="yes">
      <type>NetCType&lt; T &gt; *</type>
      <name>alloc</name>
      <anchor>e1</anchor>
      <arglist>(const T &amp;obj)</arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>Network</name>
    <filename>classNetwork.html</filename>
    <base>Node</base>
    <member kind="function">
      <type></type>
      <name>Network</name>
      <anchor>a0</anchor>
      <arglist>(string nodeName, ParameterSet params)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>~Network</name>
      <anchor>a1</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>Node *</type>
      <name>getNodeNamed</name>
      <anchor>a2</anchor>
      <arglist>(const string &amp;name)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>addNode</name>
      <anchor>a3</anchor>
      <arglist>(const string &amp;factoryName, const string &amp;nodeName, const ParameterSet &amp;parameters)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>addNode</name>
      <anchor>a4</anchor>
      <arglist>(Node &amp;node)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>connect</name>
      <anchor>a5</anchor>
      <arglist>(const string &amp;currentNodeName, const string &amp;inputName, const string &amp;inputNodeName, const string &amp;outputName)</arglist>
    </member>
    <member kind="function">
      <type>Node *</type>
      <name>removeNode</name>
      <anchor>a6</anchor>
      <arglist>(const string &amp;nodeName)</arglist>
    </member>
    <member kind="function">
      <type>string</type>
      <name>getName</name>
      <anchor>a7</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>setName</name>
      <anchor>a8</anchor>
      <arglist>(const string &amp;my_name)</arglist>
    </member>
    <member kind="function">
      <type>Node *</type>
      <name>getSinkNode</name>
      <anchor>a9</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>setSinkNode</name>
      <anchor>a10</anchor>
      <arglist>(Node *node)</arglist>
    </member>
    <member kind="function">
      <type>Node *</type>
      <name>getInputNode</name>
      <anchor>a11</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>setInputNode</name>
      <anchor>a12</anchor>
      <arglist>(Node *node)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual vector&lt; NodeInput &gt; &amp;</type>
      <name>getInputs</name>
      <anchor>a13</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>reset</name>
      <anchor>a14</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>stop</name>
      <anchor>a15</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>cleanupNotify</name>
      <anchor>a16</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>request</name>
      <anchor>a17</anchor>
      <arglist>(int outputID, const ParameterSet &amp;req)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>initialize</name>
      <anchor>a18</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual ObjectRef</type>
      <name>getOutput</name>
      <anchor>a19</anchor>
      <arglist>(int output_id, int count)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual bool</type>
      <name>hasOutput</name>
      <anchor>a20</anchor>
      <arglist>(int output_id) const </arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>connectToNode</name>
      <anchor>a21</anchor>
      <arglist>(string in, Node *inNode, string out)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>verifyConnect</name>
      <anchor>a22</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" protection="protected" virtualness="virtual">
      <type>virtual int</type>
      <name>translateInput</name>
      <anchor>b0</anchor>
      <arglist>(string inputName)</arglist>
    </member>
    <member kind="function" protection="protected" virtualness="virtual">
      <type>virtual int</type>
      <name>translateOutput</name>
      <anchor>b1</anchor>
      <arglist>(string outputName)</arglist>
    </member>
    <member kind="function" protection="protected" virtualness="virtual">
      <type>virtual void</type>
      <name>connectToNode</name>
      <anchor>b2</anchor>
      <arglist>(unsigned int in, Node *inNode, unsigned int out)</arglist>
    </member>
    <member kind="function" protection="protected">
      <type></type>
      <name>Network</name>
      <anchor>b3</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>int</type>
      <name>numNodes</name>
      <anchor>p0</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>map&lt; string, Node * &gt;</type>
      <name>nodeDictionary</name>
      <anchor>p1</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>Node *</type>
      <name>sinkNode</name>
      <anchor>p2</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>Node *</type>
      <name>inputNode</name>
      <anchor>p3</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>network_socket</name>
    <filename>classnetwork__socket.html</filename>
    <member kind="function">
      <type></type>
      <name>network_socket</name>
      <anchor>a0</anchor>
      <arglist>(int type, int port)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>~network_socket</name>
      <anchor>a1</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>printOn</name>
      <anchor>a2</anchor>
      <arglist>(ostream &amp;out) const </arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>init_broadcast</name>
      <anchor>a3</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>size_t</type>
      <name>send_packet</name>
      <anchor>a4</anchor>
      <arglist>(unsigned char *packet, size_t size)</arglist>
    </member>
    <member kind="function">
      <type>size_t</type>
      <name>recv_packet</name>
      <anchor>a5</anchor>
      <arglist>(unsigned char *packet, size_t size)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>shutdown</name>
      <anchor>a6</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>socket_connect</name>
      <anchor>a7</anchor>
      <arglist>(const char *host)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>socket_listen</name>
      <anchor>a8</anchor>
      <arglist>(int backlog, bool blocking)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>socket_accept</name>
      <anchor>a9</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>int</type>
      <name>get_type</name>
      <anchor>a10</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>int</type>
      <name>get_port</name>
      <anchor>a11</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>int</type>
      <name>get_read_socket</name>
      <anchor>a12</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>int</type>
      <name>get_write_socket</name>
      <anchor>a13</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>int</type>
      <name>get_listen_socket</name>
      <anchor>a14</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="variable" static="yes">
      <type>const int</type>
      <name>BROADCAST_TYPE</name>
      <anchor>s0</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" static="yes">
      <type>const int</type>
      <name>TCP_STREAM_TYPE</name>
      <anchor>s1</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>Node</name>
    <filename>classNode.html</filename>
    <base>Object</base>
    <member kind="function">
      <type></type>
      <name>Node</name>
      <anchor>a0</anchor>
      <arglist>(string nodeName, const ParameterSet &amp;params)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual</type>
      <name>~Node</name>
      <anchor>a1</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="pure">
      <type>virtual ObjectRef</type>
      <name>getOutput</name>
      <anchor>a2</anchor>
      <arglist>(int output_id, int count)=0</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual ObjectRef</type>
      <name>getOutputNamed</name>
      <anchor>a3</anchor>
      <arglist>(const string &amp;outputName, int count)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>connectToNode</name>
      <anchor>a4</anchor>
      <arglist>(string in, Node *inputNode, string out)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>initialize</name>
      <anchor>a5</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual bool</type>
      <name>hasOutput</name>
      <anchor>a6</anchor>
      <arglist>(int output_id) const </arglist>
    </member>
    <member kind="function">
      <type>ObjectRef</type>
      <name>getInput</name>
      <anchor>a7</anchor>
      <arglist>(int inputID, int count)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>stop</name>
      <anchor>a8</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>cleanupNotify</name>
      <anchor>a9</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>reset</name>
      <anchor>a10</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>string</type>
      <name>getName</name>
      <anchor>a11</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>request</name>
      <anchor>a12</anchor>
      <arglist>(int outputID, const ParameterSet &amp;req)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>verifyConnect</name>
      <anchor>a13</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>printOn</name>
      <anchor>a14</anchor>
      <arglist>(ostream &amp;out=cout) const </arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>setUINode</name>
      <anchor>a15</anchor>
      <arglist>(UINode *_uinode)</arglist>
    </member>
    <member kind="function" static="yes">
      <type>int</type>
      <name>addFactory</name>
      <anchor>e0</anchor>
      <arglist>(const string &amp;factoryName, _NodeFactory *const factory)</arglist>
    </member>
    <member kind="function" static="yes">
      <type>int</type>
      <name>addXPM</name>
      <anchor>e1</anchor>
      <arglist>(const string &amp;nodeName, char **XPMData)</arglist>
    </member>
    <member kind="function" static="yes">
      <type>char **</type>
      <name>getXPM</name>
      <anchor>e2</anchor>
      <arglist>(const string &amp;nodeName)</arglist>
    </member>
    <member kind="function" static="yes">
      <type>_NodeFactory *</type>
      <name>getFactoryNamed</name>
      <anchor>e3</anchor>
      <arglist>(const string &amp;name)</arglist>
    </member>
    <member kind="function" static="yes">
      <type>map&lt; string, _NodeFactory * &gt; &amp;</type>
      <name>factoryDictionary</name>
      <anchor>e4</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" static="yes">
      <type>vector&lt; string &gt; &amp;</type>
      <name>nodeInfo</name>
      <anchor>e5</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" static="yes">
      <type>map&lt; string, char ** &gt; &amp;</type>
      <name>XPMDictionary</name>
      <anchor>e6</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" static="yes">
      <type>int</type>
      <name>addNodeInfo</name>
      <anchor>e7</anchor>
      <arglist>(const string &amp;info)</arglist>
    </member>
    <member kind="function" protection="protected" virtualness="virtual">
      <type>virtual void</type>
      <name>connectToNode</name>
      <anchor>b0</anchor>
      <arglist>(unsigned int in, Node *inputNode, unsigned int out)</arglist>
    </member>
    <member kind="function" protection="protected" virtualness="virtual">
      <type>virtual int</type>
      <name>addOutput</name>
      <anchor>b1</anchor>
      <arglist>(const string &amp;outputName)</arglist>
    </member>
    <member kind="function" protection="protected" virtualness="virtual">
      <type>virtual int</type>
      <name>addInput</name>
      <anchor>b2</anchor>
      <arglist>(const string &amp;inputName)</arglist>
    </member>
    <member kind="function" protection="protected" virtualness="virtual">
      <type>virtual vector&lt; NodeInput &gt; &amp;</type>
      <name>getInputs</name>
      <anchor>b3</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" protection="protected">
      <type></type>
      <name>Node</name>
      <anchor>b4</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" protection="protected" virtualness="virtual">
      <type>virtual int</type>
      <name>translateInput</name>
      <anchor>b5</anchor>
      <arglist>(string inputName)</arglist>
    </member>
    <member kind="function" protection="protected" virtualness="virtual">
      <type>virtual int</type>
      <name>translateOutput</name>
      <anchor>b6</anchor>
      <arglist>(string inputName)</arglist>
    </member>
    <member kind="function" protection="protected" virtualness="virtual">
      <type>virtual void</type>
      <name>rt_assert</name>
      <anchor>b7</anchor>
      <arglist>(bool cond, string message=&quot;&quot;, char *_file=&quot;unknown&quot;, int _line=0)</arglist>
    </member>
    <member kind="function" protection="protected" virtualness="virtual">
      <type>virtual void</type>
      <name>construct_assert</name>
      <anchor>b8</anchor>
      <arglist>(bool cond, string message=&quot;&quot;, char *_file=&quot;unknown&quot;, int _line=0)</arglist>
    </member>
    <member kind="function" protection="protected" virtualness="virtual">
      <type>virtual void</type>
      <name>throw_error</name>
      <anchor>b9</anchor>
      <arglist>(bool send_ptr, string message, char *_file, int _line)</arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>string</type>
      <name>name</name>
      <anchor>p0</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>vector&lt; NodeInput &gt;</type>
      <name>inputs</name>
      <anchor>p1</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>vector&lt; string &gt;</type>
      <name>outputNames</name>
      <anchor>p2</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>ParameterSet</type>
      <name>parameters</name>
      <anchor>p3</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>UINode *</type>
      <name>uinode</name>
      <anchor>p4</anchor>
      <arglist></arglist>
    </member>
    <member kind="friend" protection="private">
      <type>friend class</type>
      <name>Network</name>
      <anchor>n0</anchor>
      <arglist></arglist>
    </member>
    <member kind="friend" protection="private">
      <type>friend class</type>
      <name>Iterator</name>
      <anchor>n1</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>NodeException</name>
    <filename>classNodeException.html</filename>
    <base>BaseException</base>
    <member kind="function">
      <type></type>
      <name>NodeException</name>
      <anchor>a0</anchor>
      <arglist>(Node *_node, string _message, char *_file, int _line)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>print</name>
      <anchor>a1</anchor>
      <arglist>(ostream &amp;out=cerr)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>freeze</name>
      <anchor>a2</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>string</type>
      <name>message</name>
      <anchor>p0</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>Node *</type>
      <name>node</name>
      <anchor>p1</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>string</type>
      <name>file</name>
      <anchor>p2</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>int</type>
      <name>line</name>
      <anchor>p3</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>bool</type>
      <name>frozen</name>
      <anchor>p4</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>NodeInput</name>
    <filename>classNodeInput.html</filename>
    <member kind="function">
      <type></type>
      <name>NodeInput</name>
      <anchor>a0</anchor>
      <arglist>(Node *n, int t, const string &amp;inputName)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>NodeInput</name>
      <anchor>a1</anchor>
      <arglist>(const NodeInput &amp;in)</arglist>
    </member>
    <member kind="function">
      <type>NodeInput &amp;</type>
      <name>operator=</name>
      <anchor>a2</anchor>
      <arglist>(const NodeInput &amp;in)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>NodeInput</name>
      <anchor>a3</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>NodeInput</name>
      <anchor>a4</anchor>
      <arglist>(const string &amp;inputName)</arglist>
    </member>
    <member kind="variable">
      <type>int</type>
      <name>outputID</name>
      <anchor>o0</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>Node *</type>
      <name>node</name>
      <anchor>o1</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>string</type>
      <name>name</name>
      <anchor>o2</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>NodeNotFoundException</name>
    <filename>classNodeNotFoundException.html</filename>
    <base>BaseException</base>
    <member kind="function">
      <type></type>
      <name>NodeNotFoundException</name>
      <anchor>a0</anchor>
      <arglist>(string name)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>print</name>
      <anchor>a1</anchor>
      <arglist>(ostream &amp;out=cerr)</arglist>
    </member>
    <member kind="variable">
      <type>string</type>
      <name>nodeName</name>
      <anchor>o0</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>NoInputNodeException</name>
    <filename>classNoInputNodeException.html</filename>
    <base>BaseException</base>
    <member kind="function">
      <type></type>
      <name>NoInputNodeException</name>
      <anchor>a0</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>NoInputNodeException</name>
      <anchor>a1</anchor>
      <arglist>(int value)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>print</name>
      <anchor>a2</anchor>
      <arglist>(ostream &amp;out=cerr)</arglist>
    </member>
    <member kind="variable">
      <type>int</type>
      <name>errorNo</name>
      <anchor>o0</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>NoSinkNodeException</name>
    <filename>classNoSinkNodeException.html</filename>
    <base>BaseException</base>
    <member kind="function">
      <type></type>
      <name>NoSinkNodeException</name>
      <anchor>a0</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>NoSinkNodeException</name>
      <anchor>a1</anchor>
      <arglist>(int value)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>print</name>
      <anchor>a2</anchor>
      <arglist>(ostream &amp;out=cerr)</arglist>
    </member>
    <member kind="variable">
      <type>int</type>
      <name>errorNo</name>
      <anchor>o0</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>NotInitializedException</name>
    <filename>classNotInitializedException.html</filename>
    <base>BaseException</base>
    <member kind="function">
      <type></type>
      <name>NotInitializedException</name>
      <anchor>a0</anchor>
      <arglist>(map&lt; string, Node * &gt; aMap)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>print</name>
      <anchor>a1</anchor>
      <arglist>(ostream &amp;out=cerr)</arglist>
    </member>
    <member kind="variable">
      <type>map&lt; string, Node * &gt;</type>
      <name>nodeMap</name>
      <anchor>o0</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>Object</name>
    <filename>classObject.html</filename>
    <member kind="function">
      <type></type>
      <name>Object</name>
      <anchor>a0</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual</type>
      <name>~Object</name>
      <anchor>a1</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>ref</name>
      <anchor>a2</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>unref</name>
      <anchor>a3</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>int</type>
      <name>unique</name>
      <anchor>a4</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>destroy</name>
      <anchor>a5</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>serialize</name>
      <anchor>a6</anchor>
      <arglist>(ostream &amp;out) const </arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>unserialize</name>
      <anchor>a7</anchor>
      <arglist>(istream &amp;in)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>doesNotUnderstand</name>
      <anchor>a8</anchor>
      <arglist>(string method)</arglist>
    </member>
    <member kind="function" virtualness="pure">
      <type>virtual void</type>
      <name>printOn</name>
      <anchor>a9</anchor>
      <arglist>(ostream &amp;out=cout) const =0</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual bool</type>
      <name>isNil</name>
      <anchor>a10</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>prettyPrint</name>
      <anchor>a11</anchor>
      <arglist>(ostream &amp;out=cout) const </arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>readFrom</name>
      <anchor>a12</anchor>
      <arglist>(istream &amp;in=cin)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual ObjectRef</type>
      <name>clone</name>
      <anchor>a13</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual string</type>
      <name>className</name>
      <anchor>a14</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function" static="yes">
      <type>ObjectRef</type>
      <name>newObject</name>
      <anchor>e0</anchor>
      <arglist>(const string &amp;objType)</arglist>
    </member>
    <member kind="function" static="yes">
      <type>int</type>
      <name>addObjectType</name>
      <anchor>e1</anchor>
      <arglist>(const string &amp;objType, _ObjectFactory *factory)</arglist>
    </member>
    <member kind="function" static="yes">
      <type>map&lt; string, _ObjectFactory * &gt; &amp;</type>
      <name>ObjectFactoryDictionary</name>
      <anchor>e2</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" static="yes">
      <type>TypeMap&lt; _ObjectFactory * &gt; &amp;</type>
      <name>TypeidDictionary</name>
      <anchor>e3</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>AtomicCounter</type>
      <name>ref_count</name>
      <anchor>p0</anchor>
      <arglist></arglist>
    </member>
    <member kind="friend">
      <type>friend ostream &amp;</type>
      <name>operator&lt;&lt;</name>
      <anchor>n0</anchor>
      <arglist>(ostream &amp;out, const Object &amp;obj)</arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>OStream</name>
    <filename>classOStream.html</filename>
    <base virtualness="virtual">Stream</base>
    <member kind="function">
      <type></type>
      <name>OStream</name>
      <anchor>a0</anchor>
      <arglist>(ostream *_str, bool _owner=true)</arglist>
    </member>
    <member kind="function">
      <type>OStream &amp;</type>
      <name>write</name>
      <anchor>a1</anchor>
      <arglist>(const char *ch, int len)</arglist>
    </member>
    <member kind="function">
      <type>int</type>
      <name>eof</name>
      <anchor>a2</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>int</type>
      <name>fail</name>
      <anchor>a3</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>flush</name>
      <anchor>a4</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>operator ostream &amp;</name>
      <anchor>a5</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>printOn</name>
      <anchor>a6</anchor>
      <arglist>(ostream &amp;out) const </arglist>
    </member>
    <member kind="function">
      <type>OStream &amp;</type>
      <name>seekp</name>
      <anchor>a8</anchor>
      <arglist>(int pos, ios::seekdir dir)</arglist>
    </member>
    <member kind="function">
      <type>OStream &amp;</type>
      <name>operator&lt;&lt;</name>
      <anchor>a9</anchor>
      <arglist>(const T &amp;obj)</arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>ParameterException</name>
    <filename>classParameterException.html</filename>
    <base>BaseException</base>
    <member kind="function">
      <type></type>
      <name>ParameterException</name>
      <anchor>a0</anchor>
      <arglist>(string _message, string _param_name, ParameterSet _params)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>print</name>
      <anchor>a1</anchor>
      <arglist>(ostream &amp;out=cerr)</arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>string</type>
      <name>param_name</name>
      <anchor>p0</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>ParameterSet</type>
      <name>params</name>
      <anchor>p1</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>string</type>
      <name>message</name>
      <anchor>p2</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>ParameterSet</name>
    <filename>classParameterSet.html</filename>
    <member kind="function">
      <type>bool</type>
      <name>exist</name>
      <anchor>a0</anchor>
      <arglist>(const string &amp;param) const </arglist>
    </member>
    <member kind="function">
      <type>ObjectRef</type>
      <name>get</name>
      <anchor>a1</anchor>
      <arglist>(string param) const </arglist>
    </member>
    <member kind="function">
      <type>ObjectRef</type>
      <name>getDefault</name>
      <anchor>a2</anchor>
      <arglist>(string param, ObjectRef value)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>defaultParam</name>
      <anchor>a3</anchor>
      <arglist>(string param, ObjectRef value)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>add</name>
      <anchor>a4</anchor>
      <arglist>(string param, ObjectRef value)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>print</name>
      <anchor>a5</anchor>
      <arglist>(ostream &amp;out=cerr) const </arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>checkUnused</name>
      <anchor>a6</anchor>
      <arglist>() const </arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>PrintableGenericType</name>
    <filename>classPrintableGenericType.html</filename>
    <templarg>T</templarg>
    <base>GenericType</base>
    <member kind="function">
      <type></type>
      <name>PrintableGenericType</name>
      <anchor>a0</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>PrintableGenericType</name>
      <anchor>a1</anchor>
      <arglist>(T val)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>PrintableGenericType</name>
      <anchor>a2</anchor>
      <arglist>(PrintableGenericType&lt; T &gt; &amp;copy)</arglist>
    </member>
    <member kind="function">
      <type>T &amp;</type>
      <name>val</name>
      <anchor>a3</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>printOn</name>
      <anchor>a4</anchor>
      <arglist>(ostream &amp;out) const </arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>prettyPrint</name>
      <anchor>a5</anchor>
      <arglist>(ostream &amp;out=cout) const </arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>readFrom</name>
      <anchor>a6</anchor>
      <arglist>(istream &amp;in)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>serialize</name>
      <anchor>a7</anchor>
      <arglist>(ostream &amp;out) const </arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>unserialize</name>
      <anchor>a8</anchor>
      <arglist>(istream &amp;in)</arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>PtrCastException</name>
    <filename>classPtrCastException.html</filename>
    <templarg>T</templarg>
    <templarg>U</templarg>
    <member kind="function">
      <type></type>
      <name>PtrCastException</name>
      <anchor>a0</anchor>
      <arglist>(const T *obj)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>print</name>
      <anchor>a1</anchor>
      <arglist>(ostream &amp;out=cerr)</arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>string</type>
      <name>type</name>
      <anchor>p0</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>RCPtr</name>
    <filename>classRCPtr.html</filename>
    <templarg>X</templarg>
    <member kind="typedef">
      <type>X</type>
      <name>element_type</name>
      <anchor>w0</anchor>
      <arglist></arglist>
    </member>
    <member kind="typedef">
      <type>X *</type>
      <name>pointer_type</name>
      <anchor>w1</anchor>
      <arglist></arglist>
    </member>
    <member kind="typedef">
      <type>size_t</type>
      <name>size_type</name>
      <anchor>w2</anchor>
      <arglist></arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>RCPtr</name>
      <anchor>a0</anchor>
      <arglist>(X *p=0)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>isNil</name>
      <anchor>a1</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>RCPtr</name>
      <anchor>a2</anchor>
      <arglist>(const RCPtr&lt; Z &gt; &amp;r)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>RCPtr</name>
      <anchor>a3</anchor>
      <arglist>(const RCPtr&lt; X &gt; &amp;r)</arglist>
    </member>
    <member kind="function">
      <type>RCPtr &amp;</type>
      <name>operator=</name>
      <anchor>a5</anchor>
      <arglist>(const RCPtr&lt; Z &gt; &amp;r)</arglist>
    </member>
    <member kind="function">
      <type>RCPtr &amp;</type>
      <name>operator=</name>
      <anchor>a6</anchor>
      <arglist>(const RCPtr&lt; X &gt; &amp;r)</arglist>
    </member>
    <member kind="function">
      <type>RCPtr &amp;</type>
      <name>operator=</name>
      <anchor>a7</anchor>
      <arglist>(Z *r)</arglist>
    </member>
    <member kind="function">
      <type>RCPtr &amp;</type>
      <name>operator=</name>
      <anchor>a8</anchor>
      <arglist>(X *r)</arglist>
    </member>
    <member kind="function">
      <type>X &amp;</type>
      <name>operator *</name>
      <anchor>a9</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function">
      <type>X *</type>
      <name>operator-&gt;</name>
      <anchor>a10</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function">
      <type>X *</type>
      <name>get</name>
      <anchor>a11</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>unique</name>
      <anchor>a12</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function">
      <type>X *</type>
      <name>detach</name>
      <anchor>a13</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" protection="protected">
      <type>void</type>
      <name>release</name>
      <anchor>b0</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" protection="protected">
      <type>void</type>
      <name>acquire</name>
      <anchor>b1</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>X *</type>
      <name>ptr</name>
      <anchor>p0</anchor>
      <arglist></arglist>
    </member>
    <member kind="friend" protection="protected">
      <type>friend class</type>
      <name>RCPtr</name>
      <anchor>n0</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>Score</name>
    <filename>classScore.html</filename>
    <member kind="variable">
      <type>float</type>
      <name>score</name>
      <anchor>o0</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>int</type>
      <name>gaussian_id</name>
      <anchor>o1</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>float *</type>
      <name>frame</name>
      <anchor>o2</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable">
      <type>const GMM *</type>
      <name>gmm</name>
      <anchor>o3</anchor>
      <arglist></arglist>
    </member>
    <member kind="friend">
      <type>friend class</type>
      <name>GMM</name>
      <anchor>n0</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>socket_iostream</name>
    <filename>classsocket__iostream.html</filename>
    <member kind="function">
      <type></type>
      <name>operator socket_streambuf &amp;</name>
      <anchor>a0</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>socket_iostream</name>
      <anchor>a1</anchor>
      <arglist>(int type, int port)</arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>socket_istream</name>
    <filename>classsocket__istream.html</filename>
    <member kind="function">
      <type></type>
      <name>operator socket_streambuf &amp;</name>
      <anchor>a0</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>socket_istream</name>
      <anchor>a1</anchor>
      <arglist>(int type, int port)</arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>socket_ostream</name>
    <filename>classsocket__ostream.html</filename>
    <member kind="function">
      <type></type>
      <name>operator socket_streambuf &amp;</name>
      <anchor>a0</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>socket_ostream</name>
      <anchor>a1</anchor>
      <arglist>(int type, int port)</arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>socket_streambuf</name>
    <filename>classsocket__streambuf.html</filename>
    <base>network_socket</base>
    <member kind="function">
      <type></type>
      <name>socket_streambuf</name>
      <anchor>a0</anchor>
      <arglist>(int type, int port)</arglist>
    </member>
    <member kind="function" protection="protected" virtualness="virtual">
      <type>virtual int</type>
      <name>overflow</name>
      <anchor>b0</anchor>
      <arglist>(int=EOF)</arglist>
    </member>
    <member kind="function" protection="protected" virtualness="virtual">
      <type>virtual streamsize</type>
      <name>xsputn</name>
      <anchor>b1</anchor>
      <arglist>(const char *s, streamsize n)</arglist>
    </member>
    <member kind="function" protection="protected" virtualness="virtual">
      <type>virtual int</type>
      <name>uflow</name>
      <anchor>b2</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" protection="protected" virtualness="virtual">
      <type>virtual int</type>
      <name>underflow</name>
      <anchor>b3</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" protection="protected" virtualness="virtual">
      <type>virtual streamsize</type>
      <name>xsgetn</name>
      <anchor>b4</anchor>
      <arglist>(char *s, streamsize n)</arglist>
    </member>
    <member kind="function" protection="protected" virtualness="virtual">
      <type>virtual int</type>
      <name>pbackfail</name>
      <anchor>b5</anchor>
      <arglist>(int c)</arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>bool</type>
      <name>owner</name>
      <anchor>p0</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>bool</type>
      <name>takeFromBuf</name>
      <anchor>p1</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>char</type>
      <name>charBuf</name>
      <anchor>p2</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>State</name>
    <filename>classState.html</filename>
  </compound>
  <compound kind="class">
    <name>Stream</name>
    <filename>classStream.html</filename>
    <base>Object</base>
    <member kind="function">
      <type></type>
      <name>Stream</name>
      <anchor>a0</anchor>
      <arglist>(bool _owner=true)</arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>int</type>
      <name>owner</name>
      <anchor>p0</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>String</name>
    <filename>classString.html</filename>
    <base>Object</base>
    <member kind="typedef">
      <type>string</type>
      <name>basicType</name>
      <anchor>w0</anchor>
      <arglist></arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>String</name>
      <anchor>a0</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>printOn</name>
      <anchor>a1</anchor>
      <arglist>(ostream &amp;out) const </arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>readFrom</name>
      <anchor>a2</anchor>
      <arglist>(istream &amp;in)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>serialize</name>
      <anchor>a3</anchor>
      <arglist>(ostream &amp;out) const </arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>unserialize</name>
      <anchor>a4</anchor>
      <arglist>(istream &amp;in)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>prettyPrint</name>
      <anchor>a5</anchor>
      <arglist>(ostream &amp;out) const </arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>String</name>
      <anchor>a6</anchor>
      <arglist>(const char *str)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>String</name>
      <anchor>a7</anchor>
      <arglist>(const string &amp;str)</arglist>
    </member>
    <member kind="function">
      <type>const string &amp;</type>
      <name>val</name>
      <anchor>a8</anchor>
      <arglist>()</arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>UIDocument</name>
    <filename>classUIDocument.html</filename>
    <base>Object</base>
    <member kind="function">
      <type></type>
      <name>UIDocument</name>
      <anchor>a0</anchor>
      <arglist>(string _name)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual</type>
      <name>~UIDocument</name>
      <anchor>a1</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>load</name>
      <anchor>a2</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>loadXML</name>
      <anchor>a3</anchor>
      <arglist>(xmlNodePtr root)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>loadFromMemory</name>
      <anchor>a4</anchor>
      <arglist>(const char *mem, int size)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>setCategory</name>
      <anchor>a5</anchor>
      <arglist>(const string &amp;cat)</arglist>
    </member>
    <member kind="function">
      <type>const string &amp;</type>
      <name>getCategory</name>
      <anchor>a6</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>setComments</name>
      <anchor>a7</anchor>
      <arglist>(const string &amp;comments)</arglist>
    </member>
    <member kind="function">
      <type>const string &amp;</type>
      <name>getComments</name>
      <anchor>a8</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>setModified</name>
      <anchor>a9</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>resetModified</name>
      <anchor>a10</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>isModified</name>
      <anchor>a11</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>UINetwork *</type>
      <name>addNetwork</name>
      <anchor>a12</anchor>
      <arglist>(string name, UINetwork::Type type)</arglist>
    </member>
    <member kind="function">
      <type>UINetwork *</type>
      <name>addNetwork</name>
      <anchor>a13</anchor>
      <arglist>(xmlNodePtr xmlNet)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>removeNetwork</name>
      <anchor>a14</anchor>
      <arglist>(UINetwork *toRemove)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>isUntitled</name>
      <anchor>a15</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual const string &amp;</type>
      <name>getName</name>
      <anchor>a16</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>const string &amp;</type>
      <name>getPath</name>
      <anchor>a17</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>save</name>
      <anchor>a18</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>char *</type>
      <name>saveToMemory</name>
      <anchor>a19</anchor>
      <arglist>(int &amp;size)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>addParameterText</name>
      <anchor>a20</anchor>
      <arglist>(string name, string value, string type)</arglist>
    </member>
    <member kind="function">
      <type>UINetwork *</type>
      <name>getNetworkNamed</name>
      <anchor>a21</anchor>
      <arglist>(const string &amp;n)</arglist>
    </member>
    <member kind="function">
      <type>vector&lt; ItemInfo * &gt;</type>
      <name>getNetInputs</name>
      <anchor>a22</anchor>
      <arglist>(const string &amp;netName)</arglist>
    </member>
    <member kind="function">
      <type>vector&lt; ItemInfo * &gt;</type>
      <name>getNetOutputs</name>
      <anchor>a23</anchor>
      <arglist>(const string &amp;netName)</arglist>
    </member>
    <member kind="function">
      <type>vector&lt; ItemInfo * &gt;</type>
      <name>getNetParams</name>
      <anchor>a24</anchor>
      <arglist>(const string &amp;netName)</arglist>
    </member>
    <member kind="function">
      <type>string</type>
      <name>getDescription</name>
      <anchor>a25</anchor>
      <arglist>(const string &amp;type)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>printOn</name>
      <anchor>a26</anchor>
      <arglist>(ostream &amp;out=cout) const </arglist>
    </member>
    <member kind="function">
      <type>vector&lt; UINetwork * &gt;</type>
      <name>get_networks</name>
      <anchor>a27</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>vector&lt; DocParameterDataText * &gt;</type>
      <name>get_textParams</name>
      <anchor>a28</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual UINetwork *</type>
      <name>newNetwork</name>
      <anchor>a29</anchor>
      <arglist>(const string &amp;_name, UINetwork::Type type)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual UINetwork *</type>
      <name>newNetwork</name>
      <anchor>a30</anchor>
      <arglist>(xmlNodePtr _net)</arglist>
    </member>
    <member kind="function">
      <type>Network *</type>
      <name>build</name>
      <anchor>a31</anchor>
      <arglist>(const string &amp;_name, const ParameterSet &amp;params)</arglist>
    </member>
    <member kind="function">
      <type>set&lt; string &gt;</type>
      <name>genCode</name>
      <anchor>a32</anchor>
      <arglist>(ostream &amp;out, const string &amp;functName, bool localIncludes=false)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>exportNetwork</name>
      <anchor>a33</anchor>
      <arglist>(const std::string &amp;networkName, const std::string &amp;fileName)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>importNetwork</name>
      <anchor>a34</anchor>
      <arglist>(const std::string &amp;fileName)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>run</name>
      <anchor>a35</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>run</name>
      <anchor>a36</anchor>
      <arglist>(ParameterSet &amp;p)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>setFullPath</name>
      <anchor>a37</anchor>
      <arglist>(const string &amp;fullpath)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>updateNetInfo</name>
      <anchor>a38</anchor>
      <arglist>(UINetwork *net)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>updateAllNetworks</name>
      <anchor>a39</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>updateAllSubnetTerminals</name>
      <anchor>a40</anchor>
      <arglist>(const string _nettype, const string _terminalname, UINetTerminal::NetTermType _terminaltype, bool _remove)</arglist>
    </member>
    <member kind="function">
      <type>UINodeRepository &amp;</type>
      <name>getRepository</name>
      <anchor>a41</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" static="yes">
      <type>Network *</type>
      <name>buildExternal</name>
      <anchor>e0</anchor>
      <arglist>(const string &amp;type, const string &amp;_name, const ParameterSet &amp;params)</arglist>
    </member>
    <member kind="function" static="yes">
      <type>string</type>
      <name>findExternal</name>
      <anchor>e1</anchor>
      <arglist>(const string &amp;filename, char *searchPath=&quot;FLOWDESIGNER_PATH&quot;, bool include_home=true, bool fullPathOutput=true)</arglist>
    </member>
    <member kind="function" static="yes">
      <type>void</type>
      <name>genCodeExternal</name>
      <anchor>e2</anchor>
      <arglist>(const string &amp;type, ostream &amp;out, int &amp;id, set&lt; string &gt; &amp;nodeList)</arglist>
    </member>
    <member kind="function" protection="protected" virtualness="virtual">
      <type>virtual void</type>
      <name>error</name>
      <anchor>b0</anchor>
      <arglist>(char *err)</arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>vector&lt; UINetwork * &gt;</type>
      <name>networks</name>
      <anchor>p0</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>bool</type>
      <name>modified</name>
      <anchor>p1</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>UINodeRepository</type>
      <name>subnetInfo</name>
      <anchor>p2</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>vector&lt; DocParameterDataText * &gt;</type>
      <name>textParams</name>
      <anchor>p3</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>vector&lt; ParameterText * &gt;</type>
      <name>docInputs</name>
      <anchor>p4</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>vector&lt; ParameterText * &gt;</type>
      <name>docOutputs</name>
      <anchor>p5</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>vector&lt; ParameterText * &gt;</type>
      <name>docParams</name>
      <anchor>p6</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>string</type>
      <name>docName</name>
      <anchor>p7</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>string</type>
      <name>path</name>
      <anchor>p8</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>bool</type>
      <name>untitled</name>
      <anchor>p9</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>bool</type>
      <name>destroyed</name>
      <anchor>p10</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>string</type>
      <name>category</name>
      <anchor>p11</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>string</type>
      <name>m_comments</name>
      <anchor>p12</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>UINetwork</name>
    <filename>classUINetwork.html</filename>
    <member kind="enumeration">
      <name>Type</name>
      <anchor>w3</anchor>
      <arglist></arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>UINetwork</name>
      <anchor>a0</anchor>
      <arglist>(UIDocument *_doc, string _name, Type _type)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>UINetwork</name>
      <anchor>a1</anchor>
      <arglist>(UIDocument *_doc, xmlNodePtr net, bool init=true)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>load</name>
      <anchor>a2</anchor>
      <arglist>(xmlNodePtr net)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual</type>
      <name>~UINetwork</name>
      <anchor>a3</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>setModified</name>
      <anchor>a4</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>UINode *</type>
      <name>loadNode</name>
      <anchor>a5</anchor>
      <arglist>(xmlNodePtr node)</arglist>
    </member>
    <member kind="function">
      <type>UINode *</type>
      <name>getNodeNamed</name>
      <anchor>a6</anchor>
      <arglist>(string n)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>addNode</name>
      <anchor>a7</anchor>
      <arglist>(UINode *node)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>removeNode</name>
      <anchor>a8</anchor>
      <arglist>(UINode *node)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>addLink</name>
      <anchor>a9</anchor>
      <arglist>(UILink *link)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>removeLink</name>
      <anchor>a10</anchor>
      <arglist>(UILink *link)</arglist>
    </member>
    <member kind="function">
      <type>const string &amp;</type>
      <name>getName</name>
      <anchor>a11</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>string</type>
      <name>getDescription</name>
      <anchor>a12</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>setDescription</name>
      <anchor>a13</anchor>
      <arglist>(const string &amp;_description)</arglist>
    </member>
    <member kind="function">
      <type>Type</type>
      <name>getType</name>
      <anchor>a14</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>UIDocument *</type>
      <name>getDocument</name>
      <anchor>a15</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>isIter</name>
      <anchor>a16</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>saveXML</name>
      <anchor>a17</anchor>
      <arglist>(xmlNode *root)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>newNetNotify</name>
      <anchor>a18</anchor>
      <arglist>(const string &amp;cat, const string &amp;type)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>addTerminal</name>
      <anchor>a19</anchor>
      <arglist>(UINetTerminal *term)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>removeTerminal</name>
      <anchor>a20</anchor>
      <arglist>(UINetTerminal *term)</arglist>
    </member>
    <member kind="function">
      <type>vector&lt; string &gt;</type>
      <name>getTerminals</name>
      <anchor>a21</anchor>
      <arglist>(UINetTerminal::NetTermType termType)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>insertNetParams</name>
      <anchor>a22</anchor>
      <arglist>(vector&lt; ItemInfo * &gt; &amp;params)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual UINode *</type>
      <name>newNode</name>
      <anchor>a23</anchor>
      <arglist>(UINetwork *_net, xmlNodePtr def)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual UINode *</type>
      <name>newNode</name>
      <anchor>a24</anchor>
      <arglist>(UINetwork *_net, string _name, string _type, double _x, double _y, bool doInit)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual UILink *</type>
      <name>newLink</name>
      <anchor>a25</anchor>
      <arglist>(UITerminal *_from, UITerminal *_to, char *str=NULL)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual UINote *</type>
      <name>newNote</name>
      <anchor>a26</anchor>
      <arglist>(const std::string &amp;text, double x, double y, bool visible)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual UINetTerminal *</type>
      <name>newNetTerminal</name>
      <anchor>a27</anchor>
      <arglist>(UITerminal *_terminal, UINetTerminal::NetTermType _type, const string &amp;_name, const string &amp;_objType=&quot;any&quot;, const string &amp;_description=&quot;No description available&quot;)</arglist>
    </member>
    <member kind="function">
      <type>Network *</type>
      <name>build</name>
      <anchor>a28</anchor>
      <arglist>(const string &amp;netName, const ParameterSet &amp;params)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>genCode</name>
      <anchor>a29</anchor>
      <arglist>(ostream &amp;out, int &amp;id, set&lt; string &gt; &amp;nodeList)</arglist>
    </member>
    <member kind="function">
      <type>vector&lt; UINode * &gt;</type>
      <name>getNodes</name>
      <anchor>a30</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>vector&lt; UILink * &gt;</type>
      <name>getLinks</name>
      <anchor>a31</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>vector&lt; UINetTerminal * &gt;</type>
      <name>getTerminals</name>
      <anchor>a32</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>vector&lt; UINote * &gt;</type>
      <name>getNotes</name>
      <anchor>a33</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>addNote</name>
      <anchor>a34</anchor>
      <arglist>(UINote *note)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>removeNote</name>
      <anchor>a35</anchor>
      <arglist>(UINote *note)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>rename</name>
      <anchor>a36</anchor>
      <arglist>(string newName)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>interfaceChangeNotify</name>
      <anchor>a37</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>updateAllSubnetTerminals</name>
      <anchor>a38</anchor>
      <arglist>(const string _nettype, const string _terminalname, UINetTerminal::NetTermType _terminaltype, bool _remove)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>updateAllSubnetParameters</name>
      <anchor>a39</anchor>
      <arglist>(const string _nettype, NodeInfo *_info)</arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>bool</type>
      <name>destroyed</name>
      <anchor>p0</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>UIDocument *</type>
      <name>doc</name>
      <anchor>p1</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>string</type>
      <name>name</name>
      <anchor>p2</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>string</type>
      <name>m_description</name>
      <anchor>p3</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>Type</type>
      <name>type</name>
      <anchor>p4</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>vector&lt; UINode * &gt;</type>
      <name>nodes</name>
      <anchor>p5</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>vector&lt; UILink * &gt;</type>
      <name>links</name>
      <anchor>p6</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>vector&lt; UINetTerminal * &gt;</type>
      <name>terminals</name>
      <anchor>p7</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>vector&lt; UINote * &gt;</type>
      <name>m_notes</name>
      <anchor>p8</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>UINode</name>
    <filename>classUINode.html</filename>
    <member kind="function">
      <type></type>
      <name>UINode</name>
      <anchor>a0</anchor>
      <arglist>(UINetwork *_net, string _name, string _type, double x, double y, bool doInit=1)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>UINode</name>
      <anchor>a1</anchor>
      <arglist>(UINetwork *_net, xmlNodePtr def, bool doInit=1)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual</type>
      <name>~UINode</name>
      <anchor>a2</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>const string &amp;</type>
      <name>getName</name>
      <anchor>a3</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>const string &amp;</type>
      <name>getType</name>
      <anchor>a4</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>UINetwork *</type>
      <name>getNetwork</name>
      <anchor>a5</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>rename</name>
      <anchor>a6</anchor>
      <arglist>(const string &amp;newName)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>addTerminal</name>
      <anchor>a7</anchor>
      <arglist>(const string &amp;_name, UINetTerminal::NetTermType _type, const string &amp;_objType=&quot;any&quot;, const string &amp;_description=&quot;No description available&quot;)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>removeTerminal</name>
      <anchor>a8</anchor>
      <arglist>(const string &amp;_name, UINetTerminal::NetTermType _type)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>saveXML</name>
      <anchor>a9</anchor>
      <arglist>(xmlNode *root)</arglist>
    </member>
    <member kind="function">
      <type>UITerminal *</type>
      <name>getInputNamed</name>
      <anchor>a10</anchor>
      <arglist>(string n)</arglist>
    </member>
    <member kind="function">
      <type>UITerminal *</type>
      <name>getOutputNamed</name>
      <anchor>a11</anchor>
      <arglist>(string n)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>getPos</name>
      <anchor>a12</anchor>
      <arglist>(double &amp;xx, double &amp;yy)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>setPos</name>
      <anchor>a13</anchor>
      <arglist>(double new_x, double new_y)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>setNodeParameters</name>
      <anchor>a14</anchor>
      <arglist>(UINodeParameters *params)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>insertNetParams</name>
      <anchor>a15</anchor>
      <arglist>(vector&lt; ItemInfo * &gt; &amp;params)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>updateNetParams</name>
      <anchor>a16</anchor>
      <arglist>(vector&lt; ItemInfo * &gt; &amp;params)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>notifyError</name>
      <anchor>a17</anchor>
      <arglist>(const string &amp;message)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual UILink *</type>
      <name>newLink</name>
      <anchor>a18</anchor>
      <arglist>(UITerminal *_from, UITerminal *_to)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual UINetTerminal *</type>
      <name>newNetTerminal</name>
      <anchor>a19</anchor>
      <arglist>(UITerminal *_terminal, UINetTerminal::NetTermType _type, const string &amp;_name, const string &amp;_objType=&quot;any&quot;, const string &amp;_description=&quot;No description available&quot;)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual UINodeParameters *</type>
      <name>newNodeParameters</name>
      <anchor>a20</anchor>
      <arglist>(UINode *_node, string type)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>redraw</name>
      <anchor>a21</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>Node *</type>
      <name>build</name>
      <anchor>a22</anchor>
      <arglist>(const ParameterSet &amp;params)</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>genCode</name>
      <anchor>a23</anchor>
      <arglist>(ostream &amp;out, int &amp;id, set&lt; string &gt; &amp;nodeList)</arglist>
    </member>
    <member kind="function">
      <type>vector&lt; UITerminal * &gt;</type>
      <name>getInputs</name>
      <anchor>a24</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>vector&lt; UITerminal * &gt;</type>
      <name>getOutputs</name>
      <anchor>a25</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>UINodeParameters *</type>
      <name>getParameters</name>
      <anchor>a26</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>string</type>
      <name>getDescription</name>
      <anchor>a27</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>string</type>
      <name>getComments</name>
      <anchor>a28</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>bool</type>
      <name>destroyed</name>
      <anchor>p0</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>string</type>
      <name>name</name>
      <anchor>p1</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>UINetwork *</type>
      <name>net</name>
      <anchor>p2</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>string</type>
      <name>type</name>
      <anchor>p3</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>string</type>
      <name>description</name>
      <anchor>p4</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>double</type>
      <name>x</name>
      <anchor>p5</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>double</type>
      <name>y</name>
      <anchor>p6</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>double</type>
      <name>xtmp</name>
      <anchor>p7</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>double</type>
      <name>ytmp</name>
      <anchor>p8</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>vector&lt; UITerminal * &gt;</type>
      <name>inputs</name>
      <anchor>p9</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>vector&lt; UITerminal * &gt;</type>
      <name>outputs</name>
      <anchor>p10</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>UINodeParameters *</type>
      <name>parameters</name>
      <anchor>p11</anchor>
      <arglist></arglist>
    </member>
    <member kind="friend">
      <type>friend class</type>
      <name>UINetwork</name>
      <anchor>n0</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>UINote</name>
    <filename>classUINote.html</filename>
    <member kind="function">
      <type></type>
      <name>UINote</name>
      <anchor>a0</anchor>
      <arglist>(const std::string &amp;text, double x, double y, bool visible=true)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual</type>
      <name>~UINote</name>
      <anchor>a1</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>saveXML</name>
      <anchor>a2</anchor>
      <arglist>(xmlNode *root)</arglist>
    </member>
    <member kind="function">
      <type>std::string</type>
      <name>getText</name>
      <anchor>a3</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>setText</name>
      <anchor>a4</anchor>
      <arglist>(const std::string &amp;text)</arglist>
    </member>
    <member kind="function">
      <type>bool</type>
      <name>isVisible</name>
      <anchor>a5</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>setVisible</name>
      <anchor>a6</anchor>
      <arglist>(bool visible)</arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>std::string</type>
      <name>m_text</name>
      <anchor>p0</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>double</type>
      <name>m_x</name>
      <anchor>p1</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>double</type>
      <name>m_y</name>
      <anchor>p2</anchor>
      <arglist></arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>bool</type>
      <name>m_visible</name>
      <anchor>p3</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>UserException</name>
    <filename>classUserException.html</filename>
    <member kind="function" virtualness="virtual">
      <type>virtual</type>
      <name>~UserException</name>
      <anchor>a0</anchor>
      <arglist>()</arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>Vector</name>
    <filename>classVector.html</filename>
    <templarg>T</templarg>
    <base>BaseVector</base>
    <member kind="typedef">
      <type>T</type>
      <name>basicType</name>
      <anchor>w0</anchor>
      <arglist></arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>Vector</name>
      <anchor>a0</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>Vector</name>
      <anchor>a1</anchor>
      <arglist>(const Vector&lt; T &gt; &amp;v)</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>Vector</name>
      <anchor>a2</anchor>
      <arglist>(size_t n, const T &amp;x=T())</arglist>
    </member>
    <member kind="function">
      <type></type>
      <name>~Vector</name>
      <anchor>a3</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual size_t</type>
      <name>vsize</name>
      <anchor>a4</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual bool</type>
      <name>vempty</name>
      <anchor>a5</anchor>
      <arglist>() const </arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>prettyPrint</name>
      <anchor>a6</anchor>
      <arglist>(ostream &amp;out=cout) const </arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>printOn</name>
      <anchor>a7</anchor>
      <arglist>(ostream &amp;out) const </arglist>
    </member>
    <member kind="function">
      <type>void</type>
      <name>readFrom</name>
      <anchor>a8</anchor>
      <arglist>(istream &amp;in=cin)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>serialize</name>
      <anchor>a9</anchor>
      <arglist>(ostream &amp;out) const </arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>unserialize</name>
      <anchor>a10</anchor>
      <arglist>(istream &amp;in)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>destroy</name>
      <anchor>a11</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>string</type>
      <name>getClassName</name>
      <anchor>a12</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function">
      <type>ObjectRef</type>
      <name>range</name>
      <anchor>a13</anchor>
      <arglist>(size_t startInd, size_t endInd)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual ObjectRef</type>
      <name>getIndex</name>
      <anchor>a14</anchor>
      <arglist>(int pos)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual void</type>
      <name>setIndex</name>
      <anchor>a15</anchor>
      <arglist>(int pos, ObjectRef val)</arglist>
    </member>
    <member kind="function" virtualness="virtual">
      <type>virtual ObjectRef</type>
      <name>clone</name>
      <anchor>a16</anchor>
      <arglist>()</arglist>
    </member>
    <member kind="function" static="yes">
      <type>Vector&lt; T &gt; *</type>
      <name>alloc</name>
      <anchor>e0</anchor>
      <arglist>(size_t size)</arglist>
    </member>
    <member kind="function" static="yes">
      <type>string</type>
      <name>GetClassName</name>
      <anchor>e1</anchor>
      <arglist>()</arglist>
    </member>
  </compound>
  <compound kind="class">
    <name>VQ</name>
    <filename>classVQ.html</filename>
    <base>Object</base>
    <member kind="function">
      <type></type>
      <name>VQ</name>
      <anchor>a0</anchor>
      <arglist>(float(*_dist)(const float *, const float *, int)=euclidian)</arglist>
    </member>
    <member kind="function" virtualness="pure">
      <type>virtual int</type>
      <name>nbClasses</name>
      <anchor>a2</anchor>
      <arglist>() const =0</arglist>
    </member>
    <member kind="function" virtualness="pure">
      <type>virtual int</type>
      <name>getClassID</name>
      <anchor>a3</anchor>
      <arglist>(const float *v, float *dist_return=NULL) const =0</arglist>
    </member>
    <member kind="function" virtualness="pure">
      <type>virtual void</type>
      <name>printOn</name>
      <anchor>a4</anchor>
      <arglist>(ostream &amp;out=cout) const =0</arglist>
    </member>
    <member kind="function" virtualness="pure">
      <type>virtual void</type>
      <name>readFrom</name>
      <anchor>a5</anchor>
      <arglist>(istream &amp;in=cin)=0</arglist>
    </member>
    <member kind="function" static="yes">
      <type>float</type>
      <name>euclidian</name>
      <anchor>e0</anchor>
      <arglist>(const float *x, const float *y, int len)</arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>float(*</type>
      <name>dist</name>
      <anchor>p0</anchor>
      <arglist>)(const float *, const float *, int)</arglist>
    </member>
    <member kind="variable" protection="protected">
      <type>int</type>
      <name>length</name>
      <anchor>p1</anchor>
      <arglist></arglist>
    </member>
  </compound>
  <compound kind="dir">
    <name>audio_blocks/</name>
    <path>/home/dominic/working_area/remote/FlowDesigner/audio_blocks/</path>
    <filename>dir_000004.html</filename>
    <dir>audio_blocks/include/</dir>
  </compound>
  <compound kind="dir">
    <name>data-flow/</name>
    <path>/home/dominic/working_area/remote/FlowDesigner/data-flow/</path>
    <filename>dir_000002.html</filename>
    <dir>data-flow/include/</dir>
  </compound>
  <compound kind="dir">
    <name>effects/</name>
    <path>/home/dominic/working_area/remote/FlowDesigner/effects/</path>
    <filename>dir_000016.html</filename>
    <dir>effects/include/</dir>
  </compound>
  <compound kind="dir">
    <name>FuzzyEngine/</name>
    <path>/home/dominic/working_area/remote/FlowDesigner/FuzzyEngine/</path>
    <filename>dir_000014.html</filename>
    <dir>FuzzyEngine/include/</dir>
  </compound>
  <compound kind="dir">
    <name>HMM/</name>
    <path>/home/dominic/working_area/remote/FlowDesigner/HMM/</path>
    <filename>dir_000000.html</filename>
    <dir>HMM/include/</dir>
  </compound>
  <compound kind="dir">
    <name>effects/include/</name>
    <path>/home/dominic/working_area/remote/FlowDesigner/effects/include/</path>
    <filename>dir_000017.html</filename>
    <file>reverb.h</file>
  </compound>
  <compound kind="dir">
    <name>FuzzyEngine/include/</name>
    <path>/home/dominic/working_area/remote/FlowDesigner/FuzzyEngine/include/</path>
    <filename>dir_000015.html</filename>
    <file>FuzzyFunction.h</file>
    <file>FuzzyModel.h</file>
    <file>FuzzyOperators.h</file>
    <file>FuzzyRule.h</file>
    <file>FuzzySet.h</file>
    <file>GenericModel.h</file>
    <file>InferenceModel.h</file>
    <file>Tokenizer.h</file>
    <file>TrapezoidalFunction.h</file>
    <file>TriangularFunction.h</file>
  </compound>
  <compound kind="dir">
    <name>NNet/include/</name>
    <path>/home/dominic/working_area/remote/FlowDesigner/NNet/include/</path>
    <filename>dir_000013.html</filename>
    <file>FFLayer.h</file>
    <file>FFNet.h</file>
    <file>functions.h</file>
    <file>NNetSet.h</file>
    <file>TrainingAlgo.h</file>
  </compound>
  <compound kind="dir">
    <name>lapackflow/include/</name>
    <path>/home/dominic/working_area/remote/FlowDesigner/lapackflow/include/</path>
    <filename>dir_000011.html</filename>
    <file>f77char.h</file>
    <file>f77matrix.h</file>
    <file>fortran.h</file>
    <file>lapackflow.h</file>
  </compound>
  <compound kind="dir">
    <name>VQ/include/</name>
    <path>/home/dominic/working_area/remote/FlowDesigner/VQ/include/</path>
    <filename>dir_000009.html</filename>
    <file>Cell.h</file>
    <file>CodebookMap.h</file>
    <file>FeatureMap.h</file>
    <file>kmeans.h</file>
    <file>MMIScore.h</file>
    <file>msvq.h</file>
    <file>RBF.h</file>
    <file>vq.h</file>
  </compound>
  <compound kind="dir">
    <name>vflow/include/</name>
    <path>/home/dominic/working_area/remote/FlowDesigner/vflow/include/</path>
    <filename>dir_000007.html</filename>
    <file>canvas-background.h</file>
    <file>GRunContext.h</file>
    <file>GtkPlotProbe.h</file>
    <file>GUIDocument.h</file>
    <file>GUILink.h</file>
    <file>GUINetPopup.h</file>
    <file>GUINetTerminal.h</file>
    <file>GUINetwork.h</file>
    <file>GUINode.h</file>
    <file>GUINodeParameters.h</file>
    <file>GUINodeTooltip.h</file>
    <file>GUINote.h</file>
    <file>GUITerminal.h</file>
    <file>KeyPad.h</file>
    <file>misc_gui.h</file>
    <file>MultiPlotProbe.h</file>
    <file>PlotProbe.h</file>
    <file>Probe.h</file>
    <file>TextProbe.h</file>
    <file>vflow.h</file>
    <file>vflow_pref.h</file>
  </compound>
  <compound kind="dir">
    <name>audio_blocks/include/</name>
    <path>/home/dominic/working_area/remote/FlowDesigner/audio_blocks/include/</path>
    <filename>dir_000005.html</filename>
    <file>audio.h</file>
    <file>fft_3dnow.h</file>
    <file>FFTWrap.h</file>
    <file>lpc.h</file>
    <file>lsp.h</file>
    <file>mdct.h</file>
    <file>stack_alloc.h</file>
    <file>window.h</file>
  </compound>
  <compound kind="dir">
    <name>data-flow/include/</name>
    <path>/home/dominic/working_area/remote/FlowDesigner/data-flow/include/</path>
    <filename>dir_000003.html</filename>
    <file>Array.h</file>
    <file>BaseException.h</file>
    <file>BinaryAND.h</file>
    <file>BinaryOR.h</file>
    <file>binio.h</file>
    <file>Buffer.h</file>
    <file>BufferedNode.h</file>
    <file>Collector.h</file>
    <file>compile_object.h</file>
    <file>Complex.h</file>
    <file>CompositeType.h</file>
    <file>Constant.h</file>
    <file>conversion.h</file>
    <file>cwrapper.h</file>
    <file>DLManager.h</file>
    <file>DoubleDispatch.h</file>
    <file>Exception.h</file>
    <file>ExceptionObject.h</file>
    <file>ExternalApp.h</file>
    <file>flow_pref.h</file>
    <file>flow_version.h</file>
    <file>FlowException.h</file>
    <file>fmath.h</file>
    <file>iextensions.h</file>
    <file>IntfNode.h</file>
    <file>Iterator.h</file>
    <file>Matrix.h</file>
    <file>misc.h</file>
    <file>multithread.h</file>
    <file>net_types.h</file>
    <file>Network.h</file>
    <file>Node.h</file>
    <file>NodeFactory.h</file>
    <file>Object.h</file>
    <file>object_param.h</file>
    <file>ObjectParser.h</file>
    <file>ObjectPool.h</file>
    <file>ObjectRef.h</file>
    <file>operators.h</file>
    <file>Pack.h</file>
    <file>ParameterSet.h</file>
    <file>path.h</file>
    <file>Power.h</file>
    <file>pseudosem.h</file>
    <file>rc_ptrs.h</file>
    <file>rtc.h</file>
    <file>SocketStream.h</file>
    <file>Stream.h</file>
    <file>stream_io.h</file>
    <file>stream_wrap.h</file>
    <file>sync.h</file>
    <file>ThreadedIterator.h</file>
    <file>typemap.h</file>
    <file>typetraits.h</file>
    <file>UIDocument.h</file>
    <file>UILink.h</file>
    <file>UINetTerminal.h</file>
    <file>UINetwork.h</file>
    <file>UINode.h</file>
    <file>UINodeParameters.h</file>
    <file>UINodeRepository.h</file>
    <file>UINote.h</file>
    <file>UITerminal.h</file>
    <file>UnPack.h</file>
    <file>URLHandler.h</file>
    <file>UserException.h</file>
    <file>variables.h</file>
    <file>vec.h</file>
    <file>vec_3dnow.h</file>
    <file>vec_sse.h</file>
    <file>Vector.h</file>
    <file>VectorPool.h</file>
    <file>vmethod.h</file>
    <file>wrapper.h</file>
  </compound>
  <compound kind="dir">
    <name>HMM/include/</name>
    <path>/home/dominic/working_area/remote/FlowDesigner/HMM/include/</path>
    <filename>dir_000001.html</filename>
    <file>acoustic_model.h</file>
    <file>audioinfo.h</file>
    <file>covariance.h</file>
    <file>covariance_set.h</file>
    <file>DiagGMM.h</file>
    <file>gaussian.h</file>
    <file>gaussian_set.h</file>
    <file>gmm.h</file>
    <file>gmm_set.h</file>
    <file>GMMScore.h</file>
    <file>hmm.h</file>
    <file>mean.h</file>
    <file>mean_set.h</file>
    <file>model.h</file>
    <file>state.h</file>
    <file>viterbi.h</file>
  </compound>
  <compound kind="dir">
    <name>lapackflow/</name>
    <path>/home/dominic/working_area/remote/FlowDesigner/lapackflow/</path>
    <filename>dir_000010.html</filename>
    <dir>lapackflow/include/</dir>
  </compound>
  <compound kind="dir">
    <name>NNet/</name>
    <path>/home/dominic/working_area/remote/FlowDesigner/NNet/</path>
    <filename>dir_000012.html</filename>
    <dir>NNet/include/</dir>
  </compound>
  <compound kind="dir">
    <name>vflow/</name>
    <path>/home/dominic/working_area/remote/FlowDesigner/vflow/</path>
    <filename>dir_000006.html</filename>
    <dir>vflow/include/</dir>
  </compound>
  <compound kind="dir">
    <name>VQ/</name>
    <path>/home/dominic/working_area/remote/FlowDesigner/VQ/</path>
    <filename>dir_000008.html</filename>
    <dir>VQ/include/</dir>
  </compound>
</tagfile>
