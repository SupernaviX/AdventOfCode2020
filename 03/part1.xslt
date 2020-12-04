<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="xml" indent="yes" encoding="UTF-8"/>

  <xsl:template name="process-line">
    <xsl:param name="line"/>
    <xsl:param name="column"/>
    <xsl:if test="string-length($line) &gt; 0">
      <cell>
        <xsl:attribute name="column">
          <xsl:value-of select="$column"/>
        </xsl:attribute>
        <xsl:attribute name="value">
          <xsl:value-of select="substring($line, 1, 1)"/>
        </xsl:attribute>
      </cell>
      <xsl:call-template name="process-line">
        <xsl:with-param name="line" select="substring($line, 2)"/>
        <xsl:with-param name="column" select="$column + 1"/>
      </xsl:call-template>
    </xsl:if>
  </xsl:template>

  <xsl:template name="parse-line">
    <xsl:param name="string"/>
    <xsl:param name="row"/>
    <xsl:if test="string-length($string) &gt; 0">
      <row>
        <xsl:attribute name="index">
          <xsl:value-of select="$row"/>
        </xsl:attribute>
        <xsl:call-template name="process-line">
          <xsl:with-param name="line" select="substring-before($string, '&#10;')"/>
          <xsl:with-param name="column" select="0"/>
        </xsl:call-template>
      </row>
      <xsl:call-template name="parse-line">
        <xsl:with-param name="string" select="substring-after($string, '&#10;')"/>
        <xsl:with-param name="row" select="$row + 1"/>
      </xsl:call-template>
    </xsl:if>
  </xsl:template>

  <xsl:template match="input">
    <xsl:variable name="rows">
      <xsl:call-template name="parse-line">
        <xsl:with-param name="string" select="."/>
        <xsl:with-param name="row" select="0"/>
      </xsl:call-template>
    </xsl:variable>

    <xsl:variable name="hspeed">3</xsl:variable>
    <xsl:variable name="width">
      <xsl:value-of select="count($rows/row[1]/cell)"/>
    </xsl:variable>
    <output>
      <xsl:value-of select="count($rows/row/cell[@column=(../@index * $hspeed) mod $width and @value='#'])"/>
    </output>
    <!--
    <xsl:for-each select="$rows/row/cell[@column=(../@index * $hspeed) mod $width and @value='#']">
      <xsl:copy-of select="."/>
    </xsl:for-each>
    <xsl:copy-of select="$rows"/>
    -->
  </xsl:template>
</xsl:stylesheet>