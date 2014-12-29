<?xml version="1.0" encoding="UTF-8" ?>
<!-- Released under CC lincense http://creativecommons.org/licenses/by/2.5/ -->
<!-- Feeds generated using this stylesheet (or it's derivatives) must put http://atom.geekhood.net in <generator> element -->
<x:stylesheet version="1.0" 
	exclude-result-prefixes="atom xhtml php"
	xmlns:php="http://php.net/xsl"
	xmlns:x="http://www.w3.org/1999/XSL/Transform" 
	xmlns:atom="http://www.w3.org/2005/Atom"
	xmlns:dc="http://purl.org/dc/elements/1.1/"
	xmlns:xhtml="http://www.w3.org/1999/xhtml">
<x:output encoding="utf-8" indent="yes" method="xml" media-type="application/rss+xml" />

<x:template match="/atom:feed">
	<rss version="2.0">
		<channel>
			<x:if test="//@xml:lang">
				<language><x:value-of select="//@xml:lang[1]" /></language>
			</x:if>
			
			<description>
				<x:choose>
					<x:when test="atom:subtitle"><x:apply-templates select="atom:subtitle" mode="asHTML"/></x:when>
					<x:otherwise>
						<!-- this sucks -->
						<x:apply-templates select="atom:title" mode="asHTML"/> (converted from Atom 1.0)
					</x:otherwise>
				</x:choose>
			</description>
		
			<x:choose>
				<x:when test="atom:logo"><image><x:apply-templates select="atom:logo" mode="image"/></image></x:when>
				<x:when test="atom:icon"><image><x:apply-templates select="atom:icon" mode="image"/></image></x:when>
			</x:choose>
			
			<x:apply-templates />
			<generator><x:if test="atom:generator"><x:apply-templates select="atom:generator" mode="gen"/> + </x:if>Atom 1.0 XSLT+PHP Transform v1.1 (http://atom.geekhood.net)</generator>
		</channel>
	</rss>
</x:template>


<!-- simple losless conversions -->
<x:template match="atom:contributor">
	<dc:contributor><x:call-template name="person" /></dc:contributor>
</x:template>

<x:template match="atom:title">
	<title><x:call-template name="asHTML" /></title>
</x:template>

<x:template match="atom:generator"/>
<x:template match="atom:generator" mode="gen">
	<x:apply-templates mode="asText"/> <x:if test="@version"> v<x:value-of select="@version"/></x:if> <x:if test="@uri"> (<x:value-of select="@uri"/>)</x:if>
</x:template>

<x:template match="atom:published|atom:updated">
	<x:if test="local-name() = 'published' or not(../atom:published)">
		<pubDate>
			<x:choose>
				<x:when test="function-available('php:function')">
					<x:variable name="time" select="php:function('strtotime', string(.))"/>
					<x:value-of select="php:function('gmstrftime', '%a, %d %b %Y %T %Z', $time)"/>
				</x:when>
 				<x:otherwise>
					<x:apply-templates/>
				</x:otherwise>
			</x:choose>
		</pubDate>
	</x:if>
</x:template>

<x:template match="atom:rights"><copyright><x:apply-templates /></copyright></x:template>

<x:template match="atom:link[not(@rel) or @rel='alternate']">
	<x:if test="generate-id(.) = generate-id(../atom:link[not(@rel) or @rel='alternate'][1])">
		<link><x:value-of select="@href"/></link>
	</x:if>
</x:template>

<x:template match="atom:link[@rel='enclosure']">
	<x:if test="generate-id(.) = generate-id(../atom:link[@rel='enclosure'][1])">
		<enclosure url="{@href}" type="{@type}" length="{@length}"/>
	</x:if>
</x:template>

<x:template match="atom:entry/atom:id">
	<guid>
		<x:if test="not(. = ../atom:link[@rel='alternate']/@href) and not(. = ../atom:link[@rel='permalink']/@href) and not(. = ../atom:link[not(@rel)]/@href)">
			<x:attribute name="isPermaLink">false</x:attribute>
		</x:if>
		<x:apply-templates />
	</guid>
</x:template>

<!-- dodgy conversions -->
<x:template match="atom:icon|atom:logo"/><!-- merged into rss:image -->
<x:template match="atom:icon|atom:logo" mode="image">
	<url><x:value-of select="."/></url>
	<x:if test="../atom:title"><title><x:apply-templates select="../atom:title" mode="asText" /></title></x:if>
	<x:if test="../atom:link[not(@rel) or @rel='alternate']"><link><x:value-of select="../atom:link[not(@rel) or @rel='alternate'][1]/@href"/></link></x:if>
</x:template>

<x:template name="person">
	<x:choose>
		<x:when test="atom:email"><x:value-of select="atom:email"/></x:when>
		<x:when test="/atom:feed/atom:author[./atom:email][1]/atom:email"><x:value-of select="/atom:feed/atom:author[./atom:email][1]/atom:email"/></x:when>
	</x:choose>
	 (<x:value-of select="atom:name"/><x:if test="@uri"> <x:value-of select="uri"/></x:if>)
</x:template>
	
<x:template match="atom:author[not(starts-with(./atom:email,'webmaster@'))][1]">
	<x:if test="atom:email">
		<managingEditor>
			<x:call-template name="person" />
		</managingEditor>
	</x:if>
</x:template>

<x:template match="atom:author[starts-with(./atom:email,'webmaster@')][1]">
	<webMaster>
		<x:call-template name="person" />
	</webMaster>
</x:template>

<x:template match="atom:subtitle"/>

<x:template match="atom:source">
	<source url="{atom:link[@rel='self']}"><x:apply-templates select="atom:title" mode="asText"/></source>
</x:template>

<!-- lossy conversion -->
<x:template match="atom:feed/atom:id"/>

<x:template match="atom:feed/atom:link[@rel='self']"><x:comment> source: <x:value-of select="@href"/><x:text> </x:text></x:comment></x:template>

<x:template match="atom:category"><!-- label gets lost -->
	<category>
		<x:if test="@scheme">
			<x:attribute name="domain"><x:value-of select="@scheme"/></x:attribute>
		</x:if>
		<x:value-of select="@term"/>
		<x:if test="@label and not(@label = @term)"><x:comment><x:value-of select="@label"/></x:comment></x:if>
	</category>
</x:template>


<!-- entry -->
<x:template match="atom:summary|atom:content"/>

<x:template match="atom:entry">
<item>
	<x:choose>
		<x:when test="atom:summary"><description><x:apply-templates select="atom:summary" mode="asHTML"/> <x:if test="atom:content"> (...)</x:if></description></x:when> 
		<x:when test="atom:content"><description><x:apply-templates select="atom:content" mode="asHTML"/></description></x:when> 
	</x:choose>
	<x:apply-templates/>
	<x:if test="not(atom:source) and /atom:feed/atom:link[@rel='self']">
		<source url="{/atom:feed/atom:link[@rel='self']/@href}">
			<x:choose>
				<x:when test="/atom:feed/atom:link[@rel='self' and @title]"><x:value-of select="/atom:feed/atom:link[@rel='self']/@title"/></x:when>
				<x:otherwise><x:apply-templates select="/atom:feed/atom:title" mode="asText"/></x:otherwise>
			</x:choose>
		</source>
	</x:if>
	<x:apply-templates select="atom:source/atom:category"/>
	<x:if test="not(atom:copyright)"><x:apply-templates select="atom:source/atom:copyright"/></x:if>
	<x:if test="not(atom:author)"><x:apply-templates select="atom:source/atom:author"/></x:if>
	<x:if test="not(atom:contributor)"><x:apply-templates select="atom:source/atom:contributor"/></x:if>
	<x:if test="not(atom:updated)"><x:apply-templates select="atom:source/atom:updated"/></x:if>
</item>
</x:template>


<!-- santas little helpers -->
<x:template match="*" mode="asHTML"><x:call-template name="asHTML"/></x:template>
<x:template name="asHTML">
	<x:choose>
		<x:when test="@type='xhtml'">
			<x:apply-templates select="xhtml:div" mode="xhtml2html" />
		</x:when>
		<x:when test="@type='html'"><x:value-of select="." /></x:when>
		<x:otherwise>
			<x:value-of select="."/>
		</x:otherwise>
	</x:choose>		
</x:template>

<x:template match="*[@type='html' or @type='text/html']" mode="asText">
	<x:call-template name="removeHtml"><x:with-param name="text" select="."/></x:call-template>
</x:template>

<x:template match="*" mode="asText">
	<x:value-of select="."/>
</x:template>


<!-- html 2 text (primitive method) -->
<x:template name="removeHtml">
  <x:param name="text"/>
  <x:choose>
    <x:when test="contains($text, '&lt;') and contains(substring-after($text, '&lt;'),'&gt;')">
		<x:value-of select="substring-before($text, '&lt;')"/>
		<x:call-template name="removeHtml">
  		<x:with-param name="text" select="substring-after(substring-after($text,'&lt;'),'&gt;')"/>
		</x:call-template>
    </x:when> 
    <x:otherwise>
      <x:value-of select="$text"/>  
    </x:otherwise>
 </x:choose>            
</x:template>


<!-- xhtml 2 html -->
<x:template match="xhtml:img|xhtml:br|xhtml:hr|xhtml:input|xhtml:col|xhtml:area|xhtml:input|xhtml:link|xhtml:meta|xhtml:param" mode="xhtml2html">
	&lt;<x:value-of select="local-name(.)"/><x:apply-templates select="@*" mode="xhtml2html"/>&gt;<x:apply-templates mode="xhtml2html"/>
</x:template>

<x:template match='xhtml:*' mode="xhtml2html">
	&lt;<x:value-of select="local-name(.)"/><x:apply-templates select="@*" mode="xhtml2html"/>&gt;<x:apply-templates mode="xhtml2html"/>&lt;/<x:value-of select="local-name(.)"/>&gt;
</x:template>

<x:template match='@*' mode="xhtml2html">
	<x:text> </x:text><x:value-of select="local-name(.)"/>="<x:value-of select="."/>"</x:template>

<x:template match="comment()" mode="xhtml2html">
	<x:text disable-output-escaping="yes">&lt;![CDATA[</x:text>
	<x:comment><x:value-of select="."/></x:comment>
	<x:text disable-output-escaping="yes">]]&gt;</x:text>			
</x:template>

<x:template match="text()" mode="xhtml2html">
	<x:choose>
		<x:when test="contains(.,'&amp;') or contains(.,'&lt;')">
			<x:text disable-output-escaping="yes">&lt;![CDATA[</x:text>
			<x:value-of select="." />
			<x:text disable-output-escaping="yes">]]&gt;</x:text>			
		</x:when>
		<x:otherwise>
			<x:value-of select="."/>
		</x:otherwise>
	</x:choose>
</x:template>


<!-- copy extensions -->
<x:template match='*'>
	<x:comment>Unknown element <x:value-of select="local-name(.)"/></x:comment>
	<x:copy>
  	<x:copy-of select='node()|@*'/>
	</x:copy>
</x:template>

</x:stylesheet>
