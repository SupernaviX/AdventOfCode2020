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

  <xsl:template name="count-trees">
    <xsl:param name="rows"/>
    <xsl:param name="hspeed"/>
    <xsl:param name="vspeed"/>

    <xsl:variable name="width">
      <xsl:value-of select="count($rows/row[1]/cell)"/>
    </xsl:variable>
    <xsl:variable name="real-rows">
      <xsl:copy-of select="$rows/row[@index mod $vspeed = 0]"/>
    </xsl:variable>
    <answer>
      <xsl:attribute name="value">
        <xsl:value-of select="count($real-rows/row/cell[@column=((../@index div $vspeed) * $hspeed) mod $width and @value='#'])"/>
      </xsl:attribute>
    </answer>
  </xsl:template>

  <xsl:template match="input">
    <xsl:variable name="rows">
      <xsl:call-template name="parse-line">
        <xsl:with-param name="string" select="."/>
        <xsl:with-param name="row" select="0"/>
      </xsl:call-template>
    </xsl:variable>

    <xsl:variable name="trees">
      <xsl:call-template name="count-trees">
        <xsl:with-param name="rows" select="$rows"/>
        <xsl:with-param name="hspeed" select="1"/>
        <xsl:with-param name="vspeed" select="1"/>
      </xsl:call-template>
      <xsl:call-template name="count-trees">
        <xsl:with-param name="rows" select="$rows"/>
        <xsl:with-param name="hspeed" select="3"/>
        <xsl:with-param name="vspeed" select="1"/>
      </xsl:call-template>
      <xsl:call-template name="count-trees">
        <xsl:with-param name="rows" select="$rows"/>
        <xsl:with-param name="hspeed" select="5"/>
        <xsl:with-param name="vspeed" select="1"/>
      </xsl:call-template>
      <xsl:call-template name="count-trees">
        <xsl:with-param name="rows" select="$rows"/>
        <xsl:with-param name="hspeed" select="7"/>
        <xsl:with-param name="vspeed" select="1"/>
      </xsl:call-template>
      <xsl:call-template name="count-trees">
        <xsl:with-param name="rows" select="$rows"/>
        <xsl:with-param name="hspeed" select="1"/>
        <xsl:with-param name="vspeed" select="2"/>
      </xsl:call-template>
    </xsl:variable>
    <xsl:variable name="answer">
      <xsl:value-of select="number($trees/answer[1]/@value) * number($trees/answer[2]/@value) * number($trees/answer[3]/@value) * number($trees/answer[4]/@value) * number($trees/answer[5]/@value)"/>
    </xsl:variable>
    <output>
      <xsl:value-of select="format-number($answer, 0)"/>
    </output>
  </xsl:template>
</xsl:stylesheet>