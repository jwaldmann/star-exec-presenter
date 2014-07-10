<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
    <xsl:output method="html" doctype-public="-//W3C//DTD HTML 4.01 Transitional//EN" encoding="UTF-8" doctype-system="http://www.w3.org/TR/html4/loose.dtd"/>
    <xsl:strip-space elements="*"/>
    
    <!-- TO ADAPT / INTEGRATE
      - handling of assumptions
    -->

    <xsl:variable name="cdot">&#183;</xsl:variable>
    <xsl:variable name="arrow">&#8594;</xsl:variable>
    <xsl:variable name="rewrite"> &#8594; </xsl:variable>
    <xsl:variable name="rewriteRev"> &#8592; </xsl:variable>
    <xsl:variable name="epsilon">&#949;</xsl:variable>
    <xsl:variable name="sigma">&#963;</xsl:variable>
    <xsl:variable name="pi">&#960;</xsl:variable>
    <xsl:variable name="mu">&#956;</xsl:variable>
    <xsl:variable name="infty">&#8734;</xsl:variable>
    <xsl:variable name="emptyset">&#8709;</xsl:variable>
    <xsl:variable name="forall">&#8704;</xsl:variable>
    <xsl:variable name="ge">&#8805;</xsl:variable>
    <xsl:variable name="gege">&#187;</xsl:variable>
    <xsl:variable name="box">&#9744;</xsl:variable>
    <xsl:variable name="implication">&#10233;</xsl:variable>
    
    <xsl:template match="/certificationProblem">
        <xsl:variable name="mode">
            <xsl:choose>
                <xsl:when test="proof/trsTerminationProof">Termination Proof</xsl:when>
                <xsl:when test="proof/trsNonterminationProof">Nontermination Proof</xsl:when>
                <xsl:when test="proof/dpProof">Finiteness Proof</xsl:when>
                <xsl:when test="proof/crProof">Confluence Proof</xsl:when>
                <xsl:when test="proof/crDisproof">Non-Confluence Proof</xsl:when>
                <xsl:when test="proof/completionProof">Completion Proof</xsl:when>
                <xsl:when test="proof/equationalProof">Equational Reasoning Proof</xsl:when>
                <xsl:when test="proof/equationalDisproof">Equational Reasoning Disproof</xsl:when>
                <xsl:when test="proof/dpNonterminationProof">Infiniteness Proof</xsl:when>
                <xsl:when test="proof/relativeTerminationProof">Relative Termination Proof</xsl:when>
                <xsl:when test="proof/relativeNonterminationProof">Relative Nontermination Proof</xsl:when>
                <xsl:when test="proof/complexityProof">Complexity Proof</xsl:when>
                <xsl:when test="proof/quasiReductiveProof">Quasi Reductive Proof</xsl:when>
                <xsl:when test="proof/unknownInputProof">Proof for unsupported input</xsl:when>
                <xsl:otherwise><xsl:message terminate="yes">unknown proof type</xsl:message></xsl:otherwise>
            </xsl:choose>
        </xsl:variable>
        <html>
            <head>
                <title>
                    <xsl:value-of select="$mode"/>
                </title>
                <style type="text/css">
                  .dp_fun { color: darkgreen; }
                  .error { color: red; }
                  .fun { color: darkblue; }
                  .label { color: gray; }
                  .var { color: red; }
                </style>
            </head>
            <body>
                <h1>
                    <xsl:value-of select="$mode"/>                    
                </h1>
                <xsl:apply-templates select="origin/proofOrigin"/>
                <xsl:call-template name="inputOrigin"/>
                <xsl:apply-templates select="input"/>
                
                <h2>Proof</h2>                
                    <xsl:apply-templates select="proof/*">
                        <xsl:with-param name="indent" select="1"/>
                    </xsl:apply-templates>
                <xsl:apply-templates select="origin/proofOrigin" mode="toolConfiguration"/>
            </body>
        </html>
    </xsl:template>
    
    <xsl:template match="proofOrigin">
        <p>
            <xsl:text>by </xsl:text>
            <xsl:for-each select="tool">
                <xsl:if test="position() != 1">, </xsl:if>
                <xsl:call-template name="toolName"/>
            </xsl:for-each>            
        </p>
    </xsl:template>
    
    <xsl:template name="toolName">
        <xsl:choose>
            <xsl:when test="url">
                <xsl:element name="a">
                    <xsl:attribute name="href">
                        <xsl:value-of select="normalize-space(url/text())"/>
                    </xsl:attribute>
                    <xsl:value-of select="name/text()"/>
                </xsl:element>
            </xsl:when>
            <xsl:otherwise>
                <xsl:value-of select="name/text()"/>
            </xsl:otherwise>
        </xsl:choose>        
    </xsl:template>
    
    <xsl:template match="proofOrigin" mode="toolConfiguration">
        <h2>Tool configuration</h2>
        <xsl:for-each select="tool">
            <p>
                <xsl:call-template name="toolName"/>
                <ul>
                    <li>version: <xsl:apply-templates select="version"/></li>
                    <xsl:if test="strategy">
                        <li>strategy:                         
                        <xsl:apply-templates select="strategy/text()"/>
                        </li>
                    </xsl:if>                                    
                </ul>
            </p>
        </xsl:for-each>                    
    </xsl:template>
    
    
    <xsl:template name="inputOrigin">
        <h2>
            <xsl:text>Input</xsl:text>
            <xsl:apply-templates select="origin/inputOrigin"/>
        </h2>
    </xsl:template>
    
    <xsl:template match="inputOrigin">
        <xsl:if test="count(*) != 0">: </xsl:if>
        <xsl:apply-templates select="tpdbReference"/>
        <xsl:if test="count(*) = 2"> / </xsl:if>
        <xsl:apply-templates select="source"/>
    </xsl:template>
    
    <xsl:template match="tpdbReference">
        <xsl:choose>
            <xsl:when test="tpdbId">
                <xsl:element name="a">
                    <xsl:attribute name="href">
                        <xsl:text>http://termcomp.uibk.ac.at/termcomp/tpdb/tpviewer.seam?tpId=</xsl:text>
                        <xsl:apply-templates select="tpdbId"/>
                    </xsl:attribute>
                    <xsl:apply-templates select="fileName"/>
                </xsl:element>
            </xsl:when>
            <xsl:otherwise>
                <xsl:apply-templates select="fileName"/>
            </xsl:otherwise>
        </xsl:choose>        
    </xsl:template>
    
    <xsl:template match="equationalProof">
        <xsl:param name="indent"/>
        <xsl:apply-templates select="*">
            <xsl:with-param name="indent" select="$indent"/>
        </xsl:apply-templates>
    </xsl:template>

    <xsl:template match="unknownInputProof">
        <xsl:param name="indent"/>
        <xsl:apply-templates select="*">
            <xsl:with-param name="indent" select="$indent"/>
        </xsl:apply-templates>
    </xsl:template>
    
    <xsl:template match="complexityProof">
        <xsl:param name="indent"/>
        <xsl:apply-templates select="*" mode="complexity">
            <xsl:with-param name="indent" select="$indent"/>
        </xsl:apply-templates>
    </xsl:template>

    <xsl:template match="equationalDisproof">
        <xsl:param name="indent"/>
        <xsl:apply-templates select="*" mode="neq">
            <xsl:with-param name="indent" select="$indent"/>
        </xsl:apply-templates>
    </xsl:template>
    
    <xsl:template match="trsTerminationProof">
        <xsl:param name="indent"/>
        <xsl:apply-templates select="*">
            <xsl:with-param name="indent" select="$indent"/>
        </xsl:apply-templates>
    </xsl:template>
    
    <xsl:template match="quasiReductiveProof">
        <xsl:param name="indent"/>
        <xsl:apply-templates select="*">
            <xsl:with-param name="indent" select="$indent"/>
        </xsl:apply-templates>
    </xsl:template>    

    <xsl:template match="crProof">
        <xsl:param name="indent"/>
        <xsl:apply-templates select="*">
            <xsl:with-param name="indent" select="$indent"/>
        </xsl:apply-templates>
    </xsl:template>

    <xsl:template match="crDisproof">
        <xsl:param name="indent"/>
        <xsl:apply-templates select="*">
            <xsl:with-param name="indent" select="$indent"/>
        </xsl:apply-templates>
    </xsl:template>
    
    <xsl:template match="wcrAndSN">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> Locally confluent and terminating</h3>
        Confluence is proven by showing local confluence and termination.
        <xsl:apply-templates select="./trsTerminationProof">
            <xsl:with-param name="indent" select="concat($indent,'.1')"/>
        </xsl:apply-templates>
        <xsl:apply-templates select="wcrProof">
            <xsl:with-param name="indent" select="concat($indent,'.2')"/>
        </xsl:apply-templates>
    </xsl:template>    

    <xsl:template match="modularityDisjoint">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> Modularity of confluence for disjoint unions</h3>
        The TRS can be decomposed as a disjoint union of R union S where R is the following 
        nonconfluent TRS. 
        <xsl:apply-templates select="trs"/>
        <xsl:apply-templates select="crDisproof">
            <xsl:with-param name="indent" select="concat($indent,'.1')"/>
        </xsl:apply-templates>
    </xsl:template>    
    
    <xsl:template match="nonWcrAndSN">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> Not locally confluent and terminating</h3>
        (Non-)Confluence is decidable since the TRS is terminating.
        <xsl:apply-templates select="./trsTerminationProof">
            <xsl:with-param name="indent" select="concat($indent,'.1')"/>
        </xsl:apply-templates>
    </xsl:template>    

    <xsl:template match="nonJoinableFork">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> Non-Joinable Fork</h3>
        The system is not confluenct due to the following forking derivations.  
        <p>            
          <xsl:apply-templates select="*[1]"/>
        </p>    
        <p>            
            <xsl:apply-templates select="*[2]"/>
        </p>            
        The two resulting terms cannot be joined for the following reason:
        <ul><xsl:apply-templates select="*[3]"/></ul>        
    </xsl:template>    
    
    <xsl:template match="distinctNormalForms">
        <li>The terms are distinct normal forms.
        </li>
    </xsl:template>
    
    <xsl:template match="capNotUnif">
        <li>When applying the cap-function on both terms (where variables may be treated like constants)
            then the resulting terms do not unify.</li>
    </xsl:template>
    
    <xsl:template match="emptyTreeAutomataIntersection">
        <li>
        The reachable terms of these two terms are approximated via the following two tree automata,
        and the tree automata have an empty intersection.
        <ul>
            <li><p>Automaton 1</p>
                <xsl:for-each select="firstAutomaton">
                    <xsl:call-template name="compatibleTreeAutomaton"/>
                </xsl:for-each>                        
            </li>
            <li><p>Automaton 2</p>
                <xsl:for-each select="secondAutomaton">
                    <xsl:call-template name="compatibleTreeAutomaton"/>
                </xsl:for-each>                        
            </li>
        </ul></li>
    </xsl:template>

    <xsl:template match="grounding">
        <li>We apply the substition <xsl:apply-templates select="substitution"/> on both terms and show that the resulting instances are not joinable.</li>
        <xsl:apply-templates select="*[2]"/>        
    </xsl:template>
    
    <xsl:template match="usableRulesNonJoin">
        <li>We take the usable rules of the first term (wrt. the TRS for the first term)
        and the usable rules of the second term (wrt. the TRS for the second term).
        Then the terms are not joinable w.r.t. the resulting TRSs.</li>
        <xsl:apply-templates/>        
    </xsl:template>
    
    <xsl:template match="argumentFilterNonJoin">
        <li>We filter all terms and rules w.r.t. the following argument filter.
        <xsl:apply-templates select="argumentFilter"/>
            Then the resulting terms are not joinable w.r.t. the resulting TRSs.</li>
        <xsl:apply-templates select="*[2]"/>        
    </xsl:template>
    
    <xsl:template match="strictDecrease">
        <li>The first mentioned term is strictly larger than the second one. Here, the following discrimination pair has
         been used w.r.t. the following interpretation.
         Moreover, the (reversed) rules are weakly decreasing.
         The disrimination pair is given by a 
        <xsl:apply-templates select="orderingConstraintProof"/>
        </li>
    </xsl:template>
    
    <xsl:template match="differentInterpretation">
        <li>The first mentioned term is different (not smaller than) the second one w.r.t. the following interpretation.
        Moreover, the (reversed) rules are (quasi)-models of the interpretation.
            <xsl:apply-templates select="model"/></li>
    </xsl:template>
    
    <xsl:template name="compatibleTreeAutomaton">
        <xsl:apply-templates select="treeAutomaton"/>
        <xsl:choose>
            <xsl:when test="criterion/compatibility">
                The automaton is closed under rewriting as it is compatible.
            </xsl:when>
            <xsl:when test="criterion/stateCompatibility">
                The automaton is closed under rewriting as it is state-compatible w.r.t. the following relation.
                <table>
                    <xsl:for-each select="criterion/stateCompatibility/relation/entry">
                        <tr>
                            <td align="right">
                                <xsl:apply-templates select="state[1]"/>
                            </td>
                            <td><xsl:value-of select="$gege"/></td>
                            <td align="left"><xsl:apply-templates select="state[2]"/></td>
                        </tr>
                    </xsl:for-each>
                </table>                
            </xsl:when>
            <xsl:when test="criterion/decisionProcedure">
                The automaton is closed under rewriting as can be seen by the decision procedure.
            </xsl:when>
            <xsl:otherwise>
                The automaton is closed under rewriting as it is compatible.
            </xsl:otherwise>
        </xsl:choose>   
        
    </xsl:template>
    
    
    <xsl:template match="orthogonal">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> (Weakly) Orthogonal</h3>
        Confluence is proven since the TRS is (weakly) orthogonal.
    </xsl:template>

    <xsl:template match="stronglyClosed">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> Strongly closed</h3>
        Confluence is proven since the TRS is strongly closed. 
        The joins can be performed within <xsl:value-of select="./text()"/> step(s).
    </xsl:template>
    
    <xsl:template match="equationalProofTree">
        <xsl:param name="indent"></xsl:param>
        <h3><xsl:value-of select="$indent"/> Equational Proof Tree</h3>
        We give an equational proof tree to show that the equation follows from the set of equations.
        <ul><li>
        <xsl:apply-templates mode="eqProofTree"/>
        </li></ul>
    </xsl:template>
    
    <xsl:template match="conversion">
        <xsl:param name="indent"></xsl:param>
        <h3><xsl:value-of select="$indent"/> Conversion</h3>
        We give a conversion which shows that the equation follows from the set of equations.
        <xsl:choose>
            <xsl:when test="equationStep">
                <table align="center">            
                    <tr><td/><td><xsl:apply-templates select="startTerm"/></td></tr>
                    <xsl:for-each select="equationStep">
                        <tr>
                           <td>                               
                               <xsl:choose>
                                   <xsl:when test="leftRight"><xsl:value-of select="$rewrite"/></xsl:when> 
                                   <xsl:when test="rightLeft"><xsl:value-of select="$rewriteRev"/></xsl:when>                                    
                               </xsl:choose>  
                           </td>
                            <td><xsl:apply-templates select="*[last()]"/></td>
                        </tr>                        
                    </xsl:for-each>
                </table>                    
            </xsl:when>
            <xsl:otherwise>
                The conversion is trivial since <xsl:apply-templates select="startTerm"/> = <xsl:apply-templates select="startTerm"/>.
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>
    
    <xsl:template mode="eqProofTree" match="*">
        <xsl:apply-templates mode="eqProofLeft" select="."/> 
        = 
        <xsl:apply-templates mode="eqProofRight" select="."/>
        <xsl:text> </xsl:text>
        <xsl:apply-templates mode="eqProofTree2" select="."/>
    </xsl:template>
    
    <xsl:template mode="eqProofTree2" match="refl">
        (refl)
    </xsl:template>

    <xsl:template mode="eqProofLeft" match="refl">
        <xsl:apply-templates select="*[1]"/>
    </xsl:template>
    
    <xsl:template mode="eqProofRight" match="refl">
        <xsl:apply-templates select="*[1]"/>
    </xsl:template>
        
    <xsl:template mode="eqProofTree2" match="assm">
        (assm using <xsl:apply-templates select="rule"/>)
    </xsl:template>
    
    <xsl:template mode="eqProofLeft" match="assm">
        <xsl:apply-templates select="rule/lhs" mode="apply_subst">
            <xsl:with-param name="subst" select="substitution"/>
        </xsl:apply-templates>
    </xsl:template>    
    
    <xsl:template mode="apply_subst" match="funapp">
        <xsl:param name="subst"/>
        <xsl:apply-templates select="*[1]"/>
        <xsl:if test="count(arg) &gt; 0">
            <xsl:text>(</xsl:text>
            <xsl:for-each select="arg">
                <xsl:apply-templates mode="apply_subst">
                    <xsl:with-param name="subst" select="$subst"/>
                </xsl:apply-templates>
                <xsl:if test="position() != last()">,</xsl:if>
            </xsl:for-each>
            <xsl:text>)</xsl:text>
        </xsl:if>
    </xsl:template>
    
    <xsl:template mode="apply_subst" match="var">
        <xsl:param name="subst"/>
        <xsl:variable name="x" select="text()"/>
        <xsl:choose>
            <xsl:when test="$subst/substEntry[*[1]/text() = $x]">
                <xsl:apply-templates select="($subst/substEntry[*[1]/text() = $x])[1]/*[2]"/>
            </xsl:when>
            <xsl:otherwise>
                <xsl:apply-templates select="."/>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>
    
    
    <xsl:template mode="eqProofRight" match="assm">
        <xsl:apply-templates select="rule/rhs"/>
    </xsl:template>
    
    <xsl:template mode="eqProofTree2" match="trans">
        (trans)
        <ul>
            <li>
                <xsl:apply-templates mode="eqProofTree" select="*[1]"/>                
            </li>
            <li>
                <xsl:apply-templates mode="eqProofTree" select="*[2]"/>                
            </li>            
        </ul>
    </xsl:template>

    <xsl:template mode="eqProofLeft" match="trans">
        <xsl:apply-templates mode="eqProofLeft" select="*[1]"/>
    </xsl:template>

    <xsl:template mode="eqProofRight" match="trans">
        <xsl:apply-templates mode="eqProofRight" select="*[2]"/>
    </xsl:template>
    
    <xsl:template mode="eqProofTree2" match="sym">
        (sym)
        <ul>
            <li>
                <xsl:apply-templates  mode="eqProofTree" select="*[1]"/>                
            </li>
        </ul>
    </xsl:template>
    
    <xsl:template mode="eqProofLeft" match="sym">
        <xsl:apply-templates mode="eqProofRight" select="*[1]"/>
    </xsl:template>

    <xsl:template mode="eqProofRight" match="sym">
        <xsl:apply-templates mode="eqProofLeft" select="*[1]"/>
    </xsl:template>
    
    <xsl:template mode="eqProofTree2" match="cong">
        (cong)
        <xsl:if test="count(*) &gt; 1"/>
        <ul>
            <xsl:for-each select="*">
                <xsl:if test="position() != 1">
                    <li><xsl:apply-templates mode="eqProofTree" select="."/></li>
                </xsl:if>                
            </xsl:for-each>
        </ul>
    </xsl:template>    
    
    <xsl:template mode="eqProofLeft" match="cong">
        <xsl:apply-templates select="*[1]"/>
        <xsl:if test="count(*) &gt; 1">
            <xsl:text>(</xsl:text>
            <xsl:for-each select="*">
                <xsl:if test="position() != 1">                    
                    <xsl:apply-templates mode="eqProofLeft" select="."/>
                    <xsl:if test="position() != last()">
                        <xsl:text>,</xsl:text>
                    </xsl:if>
                </xsl:if>                
            </xsl:for-each>
            <xsl:text>)</xsl:text>
        </xsl:if>
    </xsl:template>    

    <xsl:template mode="eqProofRight" match="cong">
        <xsl:apply-templates select="*[1]"/>
        <xsl:if test="count(*) &gt; 1">
            <xsl:text>(</xsl:text>
            <xsl:for-each select="*">
                <xsl:if test="position() != 1">                    
                    <xsl:apply-templates mode="eqProofRight" select="."/>
                    <xsl:if test="position() != last()">
                        <xsl:text>,</xsl:text>
                    </xsl:if>
                </xsl:if>                
            </xsl:for-each>
            <xsl:text>)</xsl:text>
        </xsl:if>
    </xsl:template>    
    
    
    <xsl:template match="completionAndNormalization">
        <xsl:param name="indent"></xsl:param>
        <h3><xsl:value-of select="$indent"/> Completion and Normalization Proof</h3>
        The following rules are a convergent TRS which is equivalent to the set of equations.
        Since both sides of the equation rewrite to the same normal form, the equation follows from 
        the set of equations.
        <xsl:apply-templates select="trs"/>
        <xsl:apply-templates select="completionProof">
            <xsl:with-param name="indent" select="concat($indent,'.1')"/>
        </xsl:apply-templates>        
    </xsl:template>

    <xsl:template match="completionAndNormalization" mode="neq">
        <xsl:param name="indent"></xsl:param>
        <h3><xsl:value-of select="$indent"/> Completion and Normalization Proof</h3>
        The following rules are a convergent TRS which is equivalent to the set of equations.
        Since both sides of the equation rewrite to different normal form, the equation does not follow from 
        the set of equations.
        <xsl:apply-templates select="trs"/>
        <xsl:apply-templates select="completionProof">
            <xsl:with-param name="indent" select="concat($indent,'.1')"/>
        </xsl:apply-templates>        
    </xsl:template>
    
    <xsl:template match="completionProof">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> Completion Proof</h3>
        We have to prove termination and local confluence of R, and equivalence of R and E.
        <xsl:apply-templates select="trsTerminationProof">
            <xsl:with-param name="indent" select="concat($indent,'.1')"/>
        </xsl:apply-templates>
        <xsl:apply-templates select="wcrProof">
            <xsl:with-param name="indent" select="concat($indent,'.2')"/>
        </xsl:apply-templates>
        <xsl:apply-templates select="equivalenceProof">
            <xsl:with-param name="indent" select="concat($indent,'.3')"/>
        </xsl:apply-templates>        
    </xsl:template>    
        
    <xsl:template match="equivalenceProof">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> Equivalence Proof of R and E</h3>
        <p>
            R can be simulated by E as follows.
        <xsl:apply-templates select="subsumptionProof[1]"/>
        </p>
        <p>            
            <xsl:choose>
                <xsl:when test="subsumptionProof[2]">
                    E can be simulated by R as follows.
                    <xsl:apply-templates select="subsumptionProof[2]">
                        <xsl:with-param name="rules">equations</xsl:with-param>
                        <xsl:with-param name="equations">rules</xsl:with-param>
                    </xsl:apply-templates>
                </xsl:when>
                <xsl:otherwise>
                    That E can be simulated by R can be shown by just computing normal forms of each equation in E.
                </xsl:otherwise>
            </xsl:choose>
            
            
        </p>        
    </xsl:template>
    
    <xsl:template match="wcrProof">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> Local Confluence Proof</h3>
        <xsl:apply-templates select="*[1]"/>        
    </xsl:template>
    
    <xsl:template match="joinableCriticalPairsAuto">
        All critical pairs are joinable which can be seen by computing normal forms of all critical pairs.
    </xsl:template>

    <xsl:template match="joinableCriticalPairsBFS">
        All critical pairs are joinable within 
        <xsl:value-of select="text()"/> step(s).
    </xsl:template>
    
    <xsl:template match="joinableCriticalPairs">
        <xsl:choose>
            <xsl:when test="joinableCriticalPair/rewriteSequence/rewriteStep">
                All critical pairs are joinable:
                <ul>
                    <xsl:for-each select="joinableCriticalPair">
                        <li>
                            <xsl:apply-templates select="rewriteSequence[1]/startTerm"/>
                            <xsl:for-each select="rewriteSequence[1]/rewriteStep">
                                <xsl:value-of select="$rewrite"/>
                                <xsl:apply-templates select="*[last()]"/>
                            </xsl:for-each>
                            <xsl:for-each select="rewriteSequence[2]/rewriteStep">
                                <xsl:variable name="i" select="last() - position()"/>
                                <xsl:value-of select="$rewriteRev"/>
                                <xsl:choose>
                                    <xsl:when test="$i = 0">
                                        <xsl:apply-templates select="../startTerm"/>
                                    </xsl:when>
                                    <xsl:otherwise>
                                        <xsl:apply-templates select="../rewriteSequence[$i]"/>
                                    </xsl:otherwise>
                                </xsl:choose>
                            </xsl:for-each>                            
                        </li>
                    </xsl:for-each>
                </ul>
            </xsl:when>
            <xsl:otherwise>
                There are no non-trivial critical pairs.
            </xsl:otherwise>
        </xsl:choose>        
    </xsl:template>    
    
    <xsl:template match="subsumptionProof">
        <xsl:param name="rules">rules</xsl:param>
        <xsl:param name="equations">equations</xsl:param>
            All <xsl:value-of select="$rules"/> could be derived from the <xsl:value-of select="$equations"/>
            <ul>
            <xsl:for-each select="ruleSubsumptionProof/conversion[count(equationStep) != 0]">
                    <li>
                            <xsl:apply-templates select="startTerm"/>
                            <xsl:for-each select="equationStep">
                                <xsl:choose>
                                    <xsl:when test="leftRight"><xsl:value-of select="$rewrite"/></xsl:when> 
                                    <xsl:when test="rightLeft"><xsl:value-of select="$rewriteRev"/></xsl:when>                                    
                                </xsl:choose>
                                <xsl:apply-templates select="*[last()]"/>
                            </xsl:for-each>
                        </li>
                    </xsl:for-each>
                </ul>
    </xsl:template>
    
    <xsl:template match="trsNonterminationProof">
        <xsl:param name="indent"/>
        <xsl:apply-templates select="*" mode="nonterm">
            <xsl:with-param name="indent" select="$indent"/>
        </xsl:apply-templates>
    </xsl:template>
      
    
    <xsl:template match="relativeTerminationProof">
        <xsl:param name="indent"/>
        <xsl:apply-templates select="*" mode="relative">
            <xsl:with-param name="indent" select="$indent"/>
        </xsl:apply-templates>
    </xsl:template>

    <xsl:template match="relativeNonterminationProof">
        <xsl:param name="indent"/>
        <xsl:apply-templates select="*" mode="relNonterm">
            <xsl:with-param name="indent" select="$indent"/>
        </xsl:apply-templates>
    </xsl:template>
    
    <xsl:template match="dpNonterminationProof">
        <xsl:param name="indent"/>
        <xsl:apply-templates select="*" mode="dpNonterm">
            <xsl:with-param name="indent" select="$indent"/>
        </xsl:apply-templates>
    </xsl:template>
    
    <xsl:template match="dpProof">
        <xsl:param name="indent"/>
        <xsl:apply-templates select="*">
            <xsl:with-param name="indent" select="$indent"/>
        </xsl:apply-templates>
    </xsl:template>
    
    <xsl:template match="orderingConstraintProof">
        <xsl:apply-templates select="*"/>
    </xsl:template>
    
    <xsl:template match="innermostLhss" mode="optional">
        <xsl:choose>
            <xsl:when test="count(*) = 0"/>            
            <xsl:otherwise>
                <p>Innermost rewriting w.r.t. the following left-hand sides is considered:</p>
                <table align="center">
                    <xsl:for-each select="*">
                        <tr><td align="left"><xsl:apply-templates select="."/></td></tr>
                    </xsl:for-each>
                </table>
            </xsl:otherwise>
        </xsl:choose>        
    </xsl:template>
    
    <xsl:template match="innermostLhss">
        <xsl:choose>
            <xsl:when test="count(*) = 0">
                <p>There are no lhss.</p>
            </xsl:when>
            <xsl:otherwise>
                <table align="center">
                    <xsl:for-each select="*">
                        <tr><td align="left"><xsl:apply-templates select="."/></td></tr>
                    </xsl:for-each>
                </table>
            </xsl:otherwise>
        </xsl:choose>        
    </xsl:template>
    
    <xsl:template match="strategy">
        <xsl:choose>
            <xsl:when test="innermost">
                <xsl:text>The evaluation strategy is innermost.</xsl:text>
            </xsl:when>
            <xsl:when test="innermostLhss">
                <xsl:text>The evaluation strategy is innermost w.r.t. the following set of left-hand sides.</xsl:text>
                <xsl:apply-templates select="innermostLhss"/>
            </xsl:when>
            <xsl:when test="outermost">
                <xsl:text>The evaluation strategy is outermost</xsl:text>
            </xsl:when>
            <xsl:when test="forbiddenPatterns">
                <xsl:text>The evaluation strategy is determined by the following forbidden patterns.</xsl:text>
                <xsl:apply-templates select="forbiddenPatterns"/>
            </xsl:when>
            <xsl:when test="contextSensitive">
                <xsl:text>The evaluation strategy is context sensitive with the following replacement map.</xsl:text>
                <table>
                    <xsl:for-each select="contextSensitive/replacementMapEntry">
                        <tr>
                            <td align="right"><xsl:value-of select="$mu"/>(<xsl:apply-templates select="*[1]"/>)</td>
                            <td align="center"> = </td>
                            <td align="left">{ <xsl:for-each select="position"><xsl:if test="position() &gt; 1">, </xsl:if><xsl:value-of select="text()"/></xsl:for-each> }</td>
                        </tr>
                    </xsl:for-each>
                </table>
                <xsl:apply-templates select="forbiddenPatterns"/>
            </xsl:when>
        </xsl:choose>
        
    </xsl:template>
    
    <xsl:template match="forbiddenPatterns">
        <table align="center">
            <xsl:for-each select="forbiddenPattern">
                <tr>
                    <td>(</td><td><xsl:apply-templates select="*[1]"/></td>
                    <td>,</td>
                    <td><xsl:apply-templates select="*[2]"/></td>
                    <td>,</td>
                    <td><xsl:choose>
                        <xsl:when test="above">above</xsl:when>
                        <xsl:when test="below">below</xsl:when>
                        <xsl:when test="here">here</xsl:when>
                    </xsl:choose>
                    </td>
                    <td>)</td>
                </tr>
            </xsl:for-each>
        </table>        
    </xsl:template>
    
    <xsl:template match="unknownInput">
        Unsupported input <i><xsl:value-of select="text()"/></i> 
    </xsl:template>
    
    <xsl:template match="trsInput">        
            <xsl:choose>
                <xsl:when test="relativeRules">
                    <p>The relative rewrite relation R/S is considered where R is the following TRS</p>                    
                    <xsl:apply-templates select="trs/rules"/>
                    <p>and S is the following TRS.</p>
                    <xsl:apply-templates select="relativeRules/rules"/>
                </xsl:when>
                <xsl:otherwise>
                    <p>The rewrite relation of the following TRS is considered.</p>
                        <xsl:apply-templates select="trs/rules"/>                        
                </xsl:otherwise>
            </xsl:choose>
            <xsl:apply-templates select="strategy"/>                
    </xsl:template>
    
    <xsl:template match="ctrsInput">        
            <p>The rewrite relation of the following conditional TRS is considered.</p>
            <xsl:apply-templates select="conditionalRules"/>                                        
    </xsl:template>
    
    <xsl:template match="complexityInput">
        <p>
            <xsl:choose>
                <xsl:when test="derivationalComplexity">
                    Derivational
                </xsl:when>
                <xsl:when test="runtimeComplexity">
                    Runtime
                </xsl:when>
            </xsl:choose>            
            <xsl:text>complexity of the following relation is considered.</xsl:text>
            <xsl:text>The intended complexity is O(</xsl:text>
            <xsl:apply-templates select="*[3]" mode="complexity_class"/>
            <xsl:text>).</xsl:text>
            <xsl:choose>
                <xsl:when test="runtimeComplexity">
                    The constructors are 
                    <xsl:for-each select="runtimeComplexity/signature[1]/symbol">
                        <xsl:if test="position() != 1">, </xsl:if>
                        <xsl:apply-templates select="*[1]"/>
                    </xsl:for-each> and the defined symbols are 
                    <xsl:for-each select="runtimeComplexity/signature[2]/symbol">
                        <xsl:if test="position() != 1">, </xsl:if>
                        <xsl:apply-templates select="*[1]"/>
                    </xsl:for-each>.
                </xsl:when>
                <xsl:when test="derivationalComplexity">
                    The following symbols are considered:  
                    <xsl:for-each select="derivationalComplexity/signature/symbol">
                        <xsl:if test="position() != 1">, </xsl:if>
                        <xsl:apply-templates select="*[1]"/>
                    </xsl:for-each>.
                </xsl:when>
            </xsl:choose>        
        </p>
            <xsl:apply-templates select="trsInput"/>                                        
    </xsl:template>
    
    <xsl:template mode="complexity_class" match="polynomial">
        <xsl:choose>
            <xsl:when test="text() = 0">1</xsl:when>
            <xsl:when test="text() = 1">n</xsl:when>
            <xsl:otherwise>n<sup><xsl:value-of select="text()"/></sup></xsl:otherwise>
        </xsl:choose>        
    </xsl:template>
    
    
    <xsl:template match="completionInput">
        <p> For the following equations E</p> 
            <xsl:apply-templates select="equations"/>
            <p>and the following TRS R</p>
            <xsl:apply-templates select="trs/rules"/>
            <p>it is proven that E is equivalent to R, and R is convergent.
        </p>        
    </xsl:template>

    <xsl:template match="equationalReasoningInput">
        <p> We consider the equations E</p> 
            <xsl:apply-templates select="equations"/>
         <p>   the equation
            <table align="center">
                <tr>
                    <td align="right"><xsl:apply-templates select="equation/*[1]"/></td>
                    <td align="center">=</td>
                    <td align="left"><xsl:apply-templates select="equation/*[2]"/></td>
                </tr>
            </table>
            and the question, whether the equation is a consequence of E.
        </p>        
    </xsl:template>
    
    
    <xsl:template match="dpInput">        
            <p>The DP problem (P,R) is considered where P are the following pairs</p>                    
                    <xsl:apply-templates select="dps/rules"/>
                    <p>and R is the following TRS.</p>
                    <xsl:apply-templates select="trs/rules"/>
        <xsl:if test="minimal/text() = 'true'"><p>Only minimal chains are regarded.</p> </xsl:if>
        <xsl:apply-templates select="strategy"/>                            
    </xsl:template>
    
    <xsl:template match="variableConditionViolated" mode="nonterm">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> Variable Condition Violated</h3>
        <p>The TRS violates one of the two variable conditions. Thus, it is not terminating.</p>
    </xsl:template>

    <xsl:template match="variableConditionViolated" mode="relNonterm">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> Variable Condition Violated</h3>
        <p>The relative termination problem R/S violates the variable condition. Thus, it is not relative terminating.</p>
    </xsl:template>
    
    <xsl:template match="positionInTerm">
        <xsl:choose>
            <xsl:when test="count(position) = 0">
                <xsl:value-of select="$epsilon"/>
            </xsl:when>
            <xsl:otherwise>
                <xsl:for-each select="position">
                    <xsl:if test="position() != 1">
                        <xsl:text>.</xsl:text>
                    </xsl:if>
                    <xsl:value-of select="text()"/>
                </xsl:for-each>
            </xsl:otherwise>
        </xsl:choose>       
    </xsl:template>
    
    <xsl:template match="rewriteSequence">
        <xsl:param name="strict"/>
        <xsl:param name="nonstrict"/>
        <xsl:variable name="str">
            <xsl:choose>
                <xsl:when test="$strict != ''">
                    <xsl:value-of select="$strict"/>
                    <xsl:text>,</xsl:text>
                </xsl:when>
            </xsl:choose>
        </xsl:variable>
        <xsl:variable name="nstr">
            <xsl:choose>
                <xsl:when test="$nonstrict != ''">
                    <xsl:value-of select="$nonstrict"/>
                    <xsl:text>,</xsl:text>
                </xsl:when>
            </xsl:choose>
        </xsl:variable>
        <table>
            <xsl:attribute name="align">center</xsl:attribute>
            <tr>
                <td align="right">t<sub>0</sub></td>
                <td align="center">=</td>
                <td align="left"><xsl:apply-templates select="startTerm/*"/></td>
            </tr>
            <xsl:for-each select="rewriteStep">
                <tr>
                    <td/>
                    <td align="center"><xsl:value-of select="$arrow"/><sub>
                        <xsl:choose>
                            <xsl:when test="relative"><xsl:value-of select="$nstr"/></xsl:when>
                            <xsl:otherwise><xsl:value-of select="$str"/></xsl:otherwise>
                        </xsl:choose>                        
                        <xsl:apply-templates select="positionInTerm"/>
                    </sub></td>
                    <td align="left"><xsl:apply-templates select="*[last()]"/></td>
                </tr>
            </xsl:for-each>
            <tr>
                <td/>
                <td align="center">=</td>
                <td align="left">t<sub><xsl:value-of select="count(rewriteStep)"/></sub></td>
            </tr>
        </table>
    </xsl:template>        

    <xsl:template match="nonterminatingSRS" mode="nonterm">
        <xsl:param name="indent"/>
        <xsl:call-template name="nonloopSRS">
            <xsl:with-param name="indent" select="$indent"/>
        </xsl:call-template>
    </xsl:template>
    
    <xsl:template match="string">
        <xsl:choose>
            <xsl:when test="count(*) = 0">
                <xsl:value-of select="$epsilon"/>
            </xsl:when>
            <xsl:otherwise>
                <xsl:for-each select="*">
                    <xsl:apply-templates select="."/>
                    <xsl:if test="position() &lt; last()"><xsl:text> </xsl:text></xsl:if>
                </xsl:for-each>
            </xsl:otherwise>
        </xsl:choose>
        
    </xsl:template>

    <xsl:template name="nonloopSRS">
        <xsl:param name="indent"/>
        <xsl:apply-templates select="*[2]">
            <xsl:with-param name="indent" select="$indent"/>
        </xsl:apply-templates>
    </xsl:template>
    
    <xsl:template match="selfEmbeddingOC">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> Looping derivation</h3>
        <p>There is a looping derivation.</p>                
        <p><xsl:apply-templates select="*[2]"/><xsl:text> </xsl:text><xsl:value-of select="$arrow"/><sup>+</sup><xsl:text> </xsl:text><i><xsl:apply-templates select="*[1]"/></i>
            <xsl:text> </xsl:text><xsl:apply-templates select="*[2]"/><xsl:text> </xsl:text><i><xsl:apply-templates select="*[3]"/></i></p>
        <xsl:apply-templates select="../*[1]"/>
    </xsl:template>

    <xsl:template match="selfEmbeddingDP">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> Infinite derivation</h3>
        <p>There is a self-embedding derivation structure which implies nontermination.</p>
        <p><xsl:apply-templates select="*[1]"/></p>
        <xsl:apply-templates select="../*[1]"/>
    </xsl:template>
    
    <xsl:template match="derivationPatterns">
        <p>The derivation can be derived as follows.</p>
        <ul>
            <xsl:apply-templates/>
        </ul>
    </xsl:template>

    <xsl:template match="derivationPattern">
        <xsl:apply-templates select="wordPattern[1]"/><xsl:text> </xsl:text><xsl:value-of select="$arrow"/><sup>+</sup><xsl:text> </xsl:text><xsl:apply-templates select="wordPattern[2]"/>
    </xsl:template>

    <xsl:template match="wordPattern">
        <xsl:variable name="factor"><xsl:choose>
            <xsl:when test="factor/text() = '1'"></xsl:when>
            <xsl:otherwise><xsl:value-of select="factor/text()"/></xsl:otherwise>
        </xsl:choose></xsl:variable>
        <xsl:variable name="constant"><xsl:choose>
            <xsl:when test="constant/text() = '0'"></xsl:when>
            <xsl:otherwise> + <xsl:value-of select="constant/text()"/></xsl:otherwise>
        </xsl:choose></xsl:variable>
        <xsl:if test="count(string[1]/*) &gt; 0"><xsl:apply-templates select="string[1]"/><xsl:text> </xsl:text></xsl:if>
        (<xsl:apply-templates select="string[2]"/>)<sup><xsl:value-of select="$factor"/>n<xsl:value-of select="$constant"/></sup>
        <xsl:if test="count(string[3]/*) &gt; 0"><xsl:text> </xsl:text><xsl:apply-templates select="string[3]"/></xsl:if>
    </xsl:template>
    
    <xsl:template match="overlapClosureSRS">
        <xsl:apply-templates select="string[1]"/><xsl:text> </xsl:text><xsl:value-of select="$arrow"/><sup>+</sup><xsl:text> </xsl:text><xsl:apply-templates select="string[2]"/>
    </xsl:template>
    
    <xsl:template match="derivationPatternProof">
        <li><xsl:apply-templates select="*[1]/*[1]"/>: 
        <xsl:choose>
            <xsl:when test="OC1">
                This is an original rule (OC1).
            </xsl:when>
            <xsl:when test="OC2">
                The overlap closure is obtained from the following two overlap closures (OC2). 
                <ul>
                    <li><xsl:apply-templates select="*[1]/*[2]"/></li>
                    <li><xsl:apply-templates select="*[1]/*[3]"/></li>
                </ul>
            </xsl:when>
            <xsl:when test="OC2prime">
                The overlap closure is obtained from the following two overlap closures (OC2'). 
                <ul>
                    <li><xsl:apply-templates select="*[1]/*[2]"/></li>
                    <li><xsl:apply-templates select="*[1]/*[3]"/></li>
                </ul>
            </xsl:when>
            <xsl:when test="OC3">
                The overlap closure is obtained from the following two overlap closures (OC3). 
                <ul>
                    <li><xsl:apply-templates select="*[1]/*[2]"/></li>
                    <li><xsl:apply-templates select="*[1]/*[3]"/></li>
                </ul>
            </xsl:when>
            <xsl:when test="OC3prime">
                The overlap closure is obtained from the following two overlap closures (OC3'). 
                <ul>
                    <li><xsl:apply-templates select="*[1]/*[2]"/></li>
                    <li><xsl:apply-templates select="*[1]/*[3]"/></li>
                </ul>
            </xsl:when>
            <xsl:when test="OCintoDP1">
                The derivation pattern is obtained from the following self-overlapping overlap closure (type 1)
                <ul>
                    <li><xsl:apply-templates select="*[1]/*[2]"/></li>
                </ul>
            </xsl:when>
            <xsl:when test="OCintoDP2">
                The derivation pattern is obtained from the following self-overlapping overlap closure (type 2)
                <ul>
                    <li><xsl:apply-templates select="*[1]/*[2]"/></li>
                </ul>
            </xsl:when>
            <xsl:when test="equivalent">
                The derivation pattern is equivalent to the following derivation pattern. 
                <ul>
                    <li><xsl:apply-templates select="*[1]/*[2]"/></li>
                </ul>
            </xsl:when>
            <xsl:when test="lift">
                The derivation pattern is obtained from lifting the following derivation pattern. 
                <ul>
                    <li><xsl:apply-templates select="*[1]/*[2]"/></li>
                </ul>
            </xsl:when>
            <xsl:when test="DP_OC_1_1">
                The derivation pattern is obtained from overlapping the following two derivation patterns (DP OC 1.1) 
                <ul>
                    <li><xsl:apply-templates select="*[1]/*[2]"/></li>
                    <li><xsl:apply-templates select="*[1]/*[3]"/></li>
                </ul>
            </xsl:when>
            <xsl:when test="DP_OC_1_2">
                The derivation pattern is obtained from overlapping the following two derivation patterns (DP OC 1.2) 
                <ul>
                    <li><xsl:apply-templates select="*[1]/*[2]"/></li>
                    <li><xsl:apply-templates select="*[1]/*[3]"/></li>
                </ul>
            </xsl:when>
            <xsl:when test="DP_OC_2">
                The derivation pattern is obtained from overlapping the following two derivation patterns (DP OC 2) 
                <ul>
                    <li><xsl:apply-templates select="*[1]/*[2]"/></li>
                    <li><xsl:apply-templates select="*[1]/*[3]"/></li>
                </ul>
            </xsl:when>
            <xsl:when test="DP_OC_3_1">
                The derivation pattern is obtained from overlapping the following two derivation patterns (DP OC 3.1) 
                <ul>
                    <li><xsl:apply-templates select="*[1]/*[2]"/></li>
                    <li><xsl:apply-templates select="*[1]/*[3]"/></li>
                </ul>
            </xsl:when>
            <xsl:when test="DP_OC_3_2">
                The derivation pattern is obtained from overlapping the following two derivation patterns (DP OC 3.2) 
                <ul>
                    <li><xsl:apply-templates select="*[1]/*[2]"/></li>
                    <li><xsl:apply-templates select="*[1]/*[3]"/></li>
                </ul>
            </xsl:when>
            <xsl:when test="DP_DP_1_1">
                The derivation pattern is obtained from overlapping the following two derivation patterns (DP DP 1.1) 
                <ul>
                    <li><xsl:apply-templates select="*[1]/*[2]"/></li>
                    <li><xsl:apply-templates select="*[1]/*[3]"/></li>
                </ul>
            </xsl:when>
            <xsl:when test="DP_DP_1_2">
                The derivation pattern is obtained from overlapping the following two derivation patterns (DP DP 1.2) 
                <ul>
                    <li><xsl:apply-templates select="*[1]/*[2]"/></li>
                    <li><xsl:apply-templates select="*[1]/*[3]"/></li>
                </ul>
            </xsl:when>
            <xsl:when test="DP_DP_2_1">
                The derivation pattern is obtained from overlapping the following two derivation patterns (DP DP 2.1) 
                <ul>
                    <li><xsl:apply-templates select="*[1]/*[2]"/></li>
                    <li><xsl:apply-templates select="*[1]/*[3]"/></li>
                </ul>
            </xsl:when>
            <xsl:when test="DP_DP_2_2">
                The derivation pattern is obtained from overlapping the following two derivation patterns (DP DP 2.2) 
                <ul>
                    <li><xsl:apply-templates select="*[1]/*[2]"/></li>
                    <li><xsl:apply-templates select="*[1]/*[3]"/></li>
                </ul>
            </xsl:when>            
            <xsl:otherwise>
                Unknown proof rule
            </xsl:otherwise>
        </xsl:choose>
        </li>
    </xsl:template>
    
    <xsl:template match="nonLoop" mode="nonterm">
        <xsl:param name="indent"/>
        <xsl:call-template name="nonLoop">
            <xsl:with-param name="indent" select="$indent"/>
        </xsl:call-template>
    </xsl:template>
    
    <xsl:template match="nonLoop" mode="dpNonterm">
        <xsl:param name="indent"/>
        <xsl:call-template name="nonLoop">
            <xsl:with-param name="indent" select="$indent"/>
        </xsl:call-template>
    </xsl:template>
    
    <xsl:template name="nonLoop">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> Non-Loop</h3>
        <p>An infinite (possibly non-looping) derivation has been detected due to the following pattern rule.</p>
        <xsl:apply-templates select="patternRule"/>        
    </xsl:template>
    
    <xsl:template match="patternTerm">
        <xsl:apply-templates select="*[1]"/>
        <xsl:apply-templates select="*[2]"/><sup>n</sup>
        <xsl:apply-templates select="*[3]"/>
    </xsl:template>
    
    <xsl:template match="patternRule">
        <b><xsl:apply-templates select="patternTerm[1]"/> <xsl:value-of select="$arrow"/><sup>+</sup> <xsl:apply-templates select="patternTerm[2]"/></b><br/>
        <xsl:choose>
            <xsl:when test="narrowing">
                The pattern rule is obtained by narrowing the following two pattern rules.
                <ul>
                    <li><xsl:apply-templates select="narrowing/patternRule[1]"/></li>
                    <li><xsl:apply-templates select="narrowing/patternRule[2]"/></li>
                </ul>
            </xsl:when>
            <xsl:when test="instantiation">
                The pattern rule is obtained by instantiating the following pattern rule.
                <ul>
                    <li><xsl:apply-templates select="instantiation/patternRule"/></li>
                </ul>
            </xsl:when>
            <xsl:when test="rewriting">
                The pattern rule is obtained by rewriting the following pattern rule.
                <ul>
                    <li><xsl:apply-templates select="rewriting/patternRule"/></li>
                </ul>
            </xsl:when>
            <xsl:when test="originalRule">
                The pattern rule is obtained from the original rule
                <xsl:apply-templates select="originalRule/rule"/>                
            </xsl:when>
            <xsl:when test="instantiationPumping">
                The pattern rule is obtained by instantiating the following pattern rule.
                <ul>
                    <li><xsl:apply-templates select="instantiationPumping/patternRule"/></li>
                </ul>
            </xsl:when>            
            <xsl:when test="initialPumping">
                The pattern rule is obtained by adding an initial pumping substitution from
                <ul>
                    <li>
                        <xsl:apply-templates select="initialPumping/patternRule"/>   
                    </li>
                </ul>                
            </xsl:when>
            <xsl:when test="initialPumpingContext">
                The pattern rule is obtained by adding an initial pumping and ending substitution from
                <ul>
                    <li>
                        <xsl:apply-templates select="initialPumpingContext/patternRule"/>   
                    </li>
                </ul>                
            </xsl:when>
            <xsl:when test="equivalence">
                The pattern rule is equivalent to the following pattern rule
                <ul>
                    <li>
                        <xsl:apply-templates select="equivalence/patternRule"/>   
                    </li>
                </ul>                
            </xsl:when>
        </xsl:choose>
        
    </xsl:template>
    
    <xsl:template match="loop" mode="nonterm">
        <xsl:param name="indent"/>
        <xsl:variable name="context" select="count(box) = 0"/>
        <xsl:variable name="subst" select="count(substitution/substEntry) &gt; 0"/>
        <h3><xsl:value-of select="$indent"/> Loop</h3>
        The following loop proves nontermination.            
        <p>
            <xsl:apply-templates select="rewriteSequence"/>
            where t<sub><xsl:value-of select="count(rewriteSequence/rewriteStep)"/></sub> = 
            <xsl:if test="$context">C[</xsl:if>
            <xsl:text>t</xsl:text><sub>0</sub>
            <xsl:if test="$subst"><xsl:value-of select="$sigma"/></xsl:if>
            <xsl:if test="$context">]</xsl:if>
            <xsl:if test="$subst or $context">
                and
            </xsl:if>
            <xsl:if test="$subst">
                <xsl:value-of select="$sigma"/> = <xsl:apply-templates select="substitution"/>
            </xsl:if>
            <xsl:if test="$subst and $context">
                and
            </xsl:if>            
            <xsl:if test="$context">
                C = <xsl:apply-templates select="*[last()]"/>
            </xsl:if>
        </p>
    </xsl:template>
    
    <xsl:template match="loop" mode="dpNonterm">
        <xsl:param name="indent"/>
        <xsl:variable name="context" select="count(box) = 0"/>
        <xsl:variable name="subst" select="count(substitution/substEntry) &gt; 0"/>
        <h3><xsl:value-of select="$indent"/> Loop</h3>
        The following loop proves infiniteness of the DP problem.
        <p>
            <xsl:apply-templates select="rewriteSequence">
                <xsl:with-param name="strict">P</xsl:with-param>
                <xsl:with-param name="nonstrict">R</xsl:with-param>
            </xsl:apply-templates>
            where t<sub><xsl:value-of select="count(rewriteSequence/rewriteStep)"/></sub> = 
            <xsl:if test="$context">C[</xsl:if>
            <xsl:text>t</xsl:text><sub>0</sub>
            <xsl:if test="$subst"><xsl:value-of select="$sigma"/></xsl:if>
            <xsl:if test="$context">]</xsl:if>
            <xsl:if test="$subst or $context">
                and
            </xsl:if>
            <xsl:if test="$subst">
                <xsl:value-of select="$sigma"/> = <xsl:apply-templates select="substitution"/>
            </xsl:if>
            <xsl:if test="$subst and $context">
                and
            </xsl:if>            
            <xsl:if test="$context">
                C = <xsl:apply-templates select="*[last()]"/>
            </xsl:if>
        </p>
    </xsl:template>

    <xsl:template match="loop" mode="relNonterm">
        <xsl:param name="indent"/>
        <xsl:variable name="context" select="count(box) = 0"/>
        <xsl:variable name="subst" select="count(substitution/substEntry) &gt; 0"/>
        <h3><xsl:value-of select="$indent"/> Loop</h3>
        The following loop proves that R/S is not relative terminating. 
        <p>
            <xsl:apply-templates select="rewriteSequence">
                <xsl:with-param name="strict">R</xsl:with-param>
                <xsl:with-param name="nonstrict">S</xsl:with-param>
            </xsl:apply-templates>
            where t<sub><xsl:value-of select="count(rewriteSequence/rewriteStep)"/></sub> = 
            <xsl:if test="$context">C[</xsl:if>
            <xsl:text>t</xsl:text><sub>0</sub>
            <xsl:if test="$subst"><xsl:value-of select="$sigma"/></xsl:if>
            <xsl:if test="$context">]</xsl:if>
            <xsl:if test="$subst or $context">
                and
            </xsl:if>
            <xsl:if test="$subst">
                <xsl:value-of select="$sigma"/> = <xsl:apply-templates select="substitution"/>
            </xsl:if>
            <xsl:if test="$subst and $context">
                and
            </xsl:if>            
            <xsl:if test="$context">
                C = <xsl:apply-templates select="*[last()]"/>
            </xsl:if>
        </p>
    </xsl:template>
    
    
    
    <xsl:template name="genVars">
        <xsl:param name="n"/>
        <xsl:choose>
            <xsl:when test="$n = 0"/>
            <xsl:when test="$n = 1">
                <xsl:text>(</xsl:text><span class="var">x<sub>1</sub></span><xsl:text>)</xsl:text>
            </xsl:when>
            <xsl:when test="$n = 2">
                <xsl:text>(</xsl:text><span class="var">x<sub>1</sub></span>, <span class="var">x<sub>2</sub></span><xsl:text>)</xsl:text>
            </xsl:when>
            <xsl:when test="$n = 3">
                <xsl:text>(</xsl:text><span class="var">x<sub>1</sub></span>, <span class="var">x<sub>2</sub></span>, <span class="var">x<sub>3</sub></span><xsl:text>)</xsl:text>
            </xsl:when>
            <xsl:otherwise>
                  <xsl:text>(</xsl:text><span class="var">x<sub>1</sub></span>,...,<span class="var">x<sub><xsl:value-of select="$n"/></sub></span><xsl:text>)</xsl:text>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>
    
    <xsl:template match="polynomial">
        <xsl:param name="inner">False</xsl:param>
        <xsl:choose>
            <xsl:when test="coefficient">
                <xsl:apply-templates select="coefficient"/>
            </xsl:when>
            <xsl:when test="variable">
                <span class="var">
                    x<sub><xsl:value-of select="variable/text()"/></sub>
                </span>
            </xsl:when>
            <xsl:when test="sum">
                <xsl:if test="$inner = 'True'">
                    <xsl:text>(</xsl:text>                    
                </xsl:if>
                <xsl:for-each select="sum/polynomial">
                    <xsl:apply-templates select="."/>
                    <xsl:if test="position() != last()"> + </xsl:if>                    
                </xsl:for-each>
                <xsl:if test="$inner = 'True'">
                    <xsl:text>)</xsl:text>                    
                </xsl:if>
            </xsl:when>
            <xsl:when test="product">
                <xsl:for-each select="product/polynomial">
                    <xsl:apply-templates select=".">
                        <xsl:with-param name="inner">True</xsl:with-param>
                    </xsl:apply-templates>
                    <xsl:if test="position() != last()">
                      <xsl:text> </xsl:text>
                      <xsl:value-of select="$cdot"/>
                      <xsl:text> </xsl:text>
                    </xsl:if>                    
                </xsl:for-each>
            </xsl:when>
            <xsl:when test="max">
                <xsl:text>max(</xsl:text>
                <xsl:for-each select="max/polynomial">
                    <xsl:apply-templates select="."/>
                    <xsl:if test="position() != last()">,</xsl:if>                    
                </xsl:for-each>
                <xsl:text>)</xsl:text>
            </xsl:when>
            <xsl:when test="min">
                <xsl:text>min(</xsl:text>
                <xsl:for-each select="min/polynomial">
                    <xsl:apply-templates select="."/>
                    <xsl:if test="position() != last()">,</xsl:if>                    
                </xsl:for-each>
                <xsl:text>)</xsl:text>
            </xsl:when>
            <xsl:otherwise>unknown polynomial type</xsl:otherwise>
        </xsl:choose>
        
    </xsl:template>
    
    <xsl:template match="integer">
        <xsl:value-of select="text()"/>
    </xsl:template>    

    <xsl:template match="algebraic">
        <xsl:variable name="firstnull" select="*[1]/text() = '0' or *[1]/numerator/text() = '0'"/>
        <xsl:variable name="secondnull" select="*[2]/text() = '0' or *[2]/numerator/text() = '0'"/>
        <xsl:variable name="secondone" select="*[2]/text() = '1' or (count(*[2]/numerator) &gt; 0 and *[2]/numerator/text() = *[2]/denominator/text())"/>
        <xsl:variable name="secondnegative" select="*[2]/text() &lt; 0 or (count(*[2]/numerator) &gt; 0 and *[2]/numerator/text() * *[2]/denominator/text() &lt; 0)"/>
        <xsl:variable name="brackets" select="not($firstnull) and not($secondnull)"/>
        <xsl:if test="$brackets">(</xsl:if>
        <xsl:if test="not($firstnull)">
            <xsl:apply-templates select="*[1]"/>
            <xsl:text> </xsl:text>
            <xsl:if test="not($secondnull) and not($secondnegative)">+ </xsl:if>
        </xsl:if>
        <xsl:if test="not($secondnull)">
            <xsl:if test="not($secondone)">
                <xsl:apply-templates select="*[2]"/>
                <xsl:text> </xsl:text>
                <xsl:value-of select="$cdot"/>
                <xsl:text> </xsl:text>
            </xsl:if>
            <xsl:text>sqrt(</xsl:text>
            <xsl:apply-templates select="*[3]"/>
            <xsl:text>)</xsl:text>
        </xsl:if>
        <xsl:if test="$brackets">)</xsl:if>
        <xsl:if test="$firstnull and $secondnull">0</xsl:if>
    </xsl:template>    
    
    <xsl:template match="rational">
        <xsl:value-of select="numerator/text()"/>
        <xsl:variable name="denom" select="denominator/text()"/>
        <xsl:if test="$denom != 1">
            <xsl:text>/</xsl:text>
            <xsl:value-of select="$denom"/>
        </xsl:if>        
    </xsl:template>    

    <xsl:template match="vector">
        <xsl:choose>
            <xsl:when test="count(coefficient) = 0">()</xsl:when>
            <xsl:otherwise>
                <table vertical-align="middle" style="display:inline; border-left-style:solid; border-left-width:thin; border-right-style: solid; border-right-width:thin; border-color:black; border-top-width:0;">
                    <xsl:for-each select="coefficient">
                        <tr>
                            <td>
                                <xsl:apply-templates select="."/>
                            </td>
                        </tr>
                    </xsl:for-each>
                </table>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>
    
    <xsl:template match="matrix">
        <xsl:choose>
            <xsl:when test="count(vector) = 0">()</xsl:when>
            <xsl:otherwise>
                <table vertical-align="middle" style="display:inline; border-left-style:solid; border-left-width:thin; border-right-style: solid; border-right-width:thin; border-color:black; border-top-width:0;">
                    <xsl:call-template name="matrix2">
                        <xsl:with-param name="width" select="count(vector)"/>
                        <xsl:with-param name="heigth" select="count(vector[1]/coefficient)"/>
                        <xsl:with-param name="h" select="1"/>
                    </xsl:call-template>
                </table>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>
    
    <xsl:template name="matrix2">
        <xsl:param name="heigth"/>
        <xsl:param name="width"/>
        <xsl:param name="h"/>
        <tr>
            <xsl:call-template name="matrix3">
                <xsl:with-param name="width" select="$width"/>
                <xsl:with-param name="h" select="$h"/>                
                <xsl:with-param name="w" select="1"/>                
            </xsl:call-template>
        </tr>
        <xsl:if test="$h != $heigth">
            <xsl:call-template name="matrix2">
                <xsl:with-param name="heigth" select="$heigth"/>
                <xsl:with-param name="width" select="$width"/>
                <xsl:with-param name="h" select="$h + 1"/>
            </xsl:call-template>
        </xsl:if>
    </xsl:template>

    <xsl:template name="matrix3">
        <xsl:param name="width"/>
        <xsl:param name="h"/>
        <xsl:param name="w"/>
        <td>
            <xsl:apply-templates select="vector[$w]/coefficient[$h]"/>
        </td>
        <xsl:if test="$w != $width">
            <xsl:call-template name="matrix3">
                <xsl:with-param name="width" select="$width"/>
                <xsl:with-param name="w" select="$w + 1"/>
                <xsl:with-param name="h" select="$h"/>
            </xsl:call-template>
        </xsl:if>
    </xsl:template>
    
    <xsl:template match="coefficient">        
        <xsl:choose>
            <xsl:when test="integer">
                <xsl:apply-templates select="integer"/>
            </xsl:when>
            <xsl:when test="rational">
                <xsl:apply-templates select="rational"/>
            </xsl:when>
            <xsl:when test="algebraic">
                <xsl:apply-templates select="algebraic"/>
            </xsl:when>            
            <xsl:when test="minusInfinity">
                -<xsl:value-of select="$infty"/>
            </xsl:when>
            <xsl:when test="vector">
                <xsl:apply-templates select="vector"/>
            </xsl:when>
            <xsl:when test="matrix">
                <xsl:apply-templates select="matrix"/>                
            </xsl:when>
            <xsl:otherwise>
                unknown coefficient
            </xsl:otherwise>
        </xsl:choose>        
    </xsl:template>
    
    
    <xsl:template match="domain">
        <xsl:if test="naturals">the naturals</xsl:if>
        <xsl:if test="integers">the integers</xsl:if>
        <xsl:if test="arctic">the arctic semiring over <xsl:apply-templates select="arctic/domain"/></xsl:if>
        <xsl:if test="rationals">the rationals with delta = <xsl:apply-templates select="rationals/delta/*"/></xsl:if>
        <xsl:if test="algebraicNumbers">the algebraic numbers with delta = <xsl:apply-templates select="algebraicNumbers/delta/*"/></xsl:if>
        <xsl:if test="arcticBelowZero">the integers with -<xsl:value-of select="$infty"/> in the arctic semiring</xsl:if>
        <xsl:if test="matrices">(<xsl:value-of select="matrices/dimension/text()"/> x <xsl:value-of
        select="matrices/dimension/text()"/>)-matrices with strict dimension <xsl:value-of select="matrices/strictDimension/text()"/> 
            over <xsl:apply-templates
                select="matrices/domain"/>
        </xsl:if>
    </xsl:template>
    
    <xsl:template match="type">
        <!-- currently the strict dimensions are not displayed -->
        <xsl:choose>            
            <xsl:when test="polynomial">
                <xsl:if test="polynomial/degree/text() != 1">non-</xsl:if>
                <xsl:text>linear polynomial interpretation over </xsl:text>
                <xsl:apply-templates select="polynomial/domain"/>
            </xsl:when>
            <xsl:when test="matrixInterpretation">
                <xsl:text>matrix interpretations of dimension </xsl:text> 
                <xsl:value-of select="matrixInterpretation/dimension/text()"/>
                <xsl:text> with strict dimension </xsl:text> 
                <xsl:value-of select="matrixInterpretation/strictDimension/text()"/>
                <xsl:text> over </xsl:text>
                <xsl:apply-templates select="matrixInterpretation/domain"/>            
            </xsl:when>
            <xsl:otherwise>
                some unknown interpretation type
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>
    
    <xsl:template match="interpretation">
        <xsl:apply-templates select="*[1]"/>             
            <table>
                <xsl:attribute name="align">center</xsl:attribute>
                <xsl:for-each select="interpret">
                    <tr>
                        <td><xsl:attribute name="align">right</xsl:attribute>
                            <xsl:text>[</xsl:text><xsl:apply-templates select="*[1]"/><xsl:call-template name="genVars">
                                <xsl:with-param name="n" select="arity"/>
                            </xsl:call-template><xsl:text>]</xsl:text>
                        </td>
                        <td><xsl:attribute name="align">center</xsl:attribute> = </td>
                        <td><xsl:attribute name="align">left</xsl:attribute><xsl:apply-templates select="*[3]"/></td>
                    </tr>           
                </xsl:for-each>
            </table>                      
    </xsl:template>
    
    <xsl:template match="pathOrder">
        <xsl:text>recursive path order with the following precedence and status</xsl:text>
        <xsl:apply-templates select="statusPrecedence"/>
        <xsl:if test="argumentFilter">
            in combination with the following argument filter 
            <xsl:apply-templates select="argumentFilter"/>
        </xsl:if>
    </xsl:template>
    
    <xsl:template match="statusPrecedence">
        <xsl:if test="count(statusPrecedenceEntry) != 0">
            <table align="center" width="100%">
                <xsl:for-each select="statusPrecedenceEntry">
                    <tr>
                        <td align="right">
                            <xsl:text>prec(</xsl:text>
                            <xsl:apply-templates select="*[1]"/>
                            <xsl:text>)</xsl:text>
                        </td>
                        <td align="center">=</td>
                        <td align="left">
                            <xsl:value-of select="precedence/text()"/>
                        </td>
                        <td/>
                        <td align="right">
                            <xsl:text>stat(</xsl:text>
                            <xsl:apply-templates select="*[1]"/>
                            <xsl:text>)</xsl:text>
                        </td>
                        <td align="center">=</td>
                        <td align="left">
                            <xsl:choose>
                                <xsl:when test="lex">
                                    lex
                                </xsl:when>
                                <xsl:when test="mul">
                                    mul
                                </xsl:when>
                                <xsl:otherwise>
                                    (unknown status)
                                </xsl:otherwise>
                            </xsl:choose>
                        </td>                        
                    </tr>
                </xsl:for-each>
            </table>
        </xsl:if>
    </xsl:template>

    <xsl:template match="knuthBendixOrder">
        <xsl:text>Knuth Bendix order with w0 = </xsl:text>
        <xsl:value-of select="w0/text()"/>
        <xsl:text> and the following precedence and weight function</xsl:text>
        <xsl:apply-templates select="precedenceWeight"/>
        <xsl:if test="argumentFilter">
            in combination with the following argument filter 
            <xsl:apply-templates select="argumentFilter"/>
        </xsl:if>
    </xsl:template>
    
    <xsl:template match="precedenceWeight">
        <xsl:if test="count(precedenceWeightEntry) != 0">
            <table align="center" width="100%">
                <xsl:for-each select="precedenceWeightEntry">
                    <tr>
                        <td align="right">
                            <xsl:text>prec(</xsl:text>
                            <xsl:apply-templates select="*[1]"/>
                            <xsl:text>)</xsl:text>
                        </td>
                        <td align="center">=</td>
                        <td align="left">
                            <xsl:value-of select="precedence/text()"/>
                        </td>
                        <td/>
                        <td align="right">
                            <xsl:text>weight(</xsl:text>
                            <xsl:apply-templates select="*[1]"/>
                            <xsl:text>)</xsl:text>
                        </td>
                        <td align="center">=</td>
                        <td align="left">
                            <xsl:value-of select="weight/text()"/>
                        </td>
                        <xsl:choose>
                            <xsl:when test="subtermCoefficientEntries">
                                <td align="right">
                                    <xsl:text>subterm-coefficients(</xsl:text>
                                    <xsl:apply-templates select="*[1]"/>
                                    <xsl:text>)</xsl:text>
                                </td>
                                <td align="center">=</td>
                                <td align="left">
                                    <xsl:text>[</xsl:text>
                                    <xsl:for-each select="subtermCoefficientEntries/entry">
                                        <xsl:if test="position() != 1">, </xsl:if>
                                        <xsl:value-of select="./text()"/>
                                    </xsl:for-each>
                                    <xsl:text>]</xsl:text>
                                </td>                                
                            </xsl:when>
                            <xsl:otherwise>
                                <td/><td/><td/>
                            </xsl:otherwise>
                        </xsl:choose>
                        
                    </tr>
                </xsl:for-each>
            </table>
        </xsl:if>
    </xsl:template>
    
    <xsl:template match="levelMapping">
        <table align="center" width="100%">
            <xsl:for-each select="levelMappingEntry">
                <tr>
                    <td align="right">
                        <xsl:value-of select="$pi"/><xsl:text>(</xsl:text>
                        <xsl:apply-templates select="*[1]"/>
                        <xsl:text>)</xsl:text>
                    </td>
                    <td align="center">=</td>
                    <td align="left">
                        {
                        <xsl:for-each select="positionLevelEntry">
                            <xsl:if test="position() != 1">, </xsl:if>
                            <xsl:text>&lt;</xsl:text>
                            <xsl:variable name="p" select="position/text()"/>
                            <xsl:choose>
                                <xsl:when test="$p = '0'"><xsl:value-of select="$epsilon"/></xsl:when>
                                <xsl:otherwise><xsl:value-of select="$p"/></xsl:otherwise>
                            </xsl:choose>
                            <xsl:text>,</xsl:text>
                            <xsl:value-of select="level/text()"/>
                            <xsl:text>&gt;</xsl:text>
                        </xsl:for-each>
                        }
                    </td>
                </tr>
            </xsl:for-each>
        </table>        
    </xsl:template>
    
    <xsl:template match="scnp">
        SCNP-reduction pair with <xsl:value-of select="$mu"/> = 
        <xsl:choose>
            <xsl:when test="status/max">max</xsl:when>
            <xsl:when test="status/min">min</xsl:when>
            <xsl:when test="status/ms">ms</xsl:when>
            <xsl:when test="status/dms">dms</xsl:when>
            <xsl:otherwise>(unknown status)</xsl:otherwise>
        </xsl:choose>
        and level-mapping
        <xsl:apply-templates select="levelMapping"/>
        based on the reduction pair
        <xsl:apply-templates select="redPair"/>
    </xsl:template>
    
    <xsl:template match="redPair">
        <xsl:choose>
            <xsl:when test="interpretation">
                <xsl:apply-templates/>
            </xsl:when>
            <xsl:when test="pathOrder">
                <xsl:apply-templates/> 
            </xsl:when>
            <xsl:when test="knuthBendixOrder">
                <xsl:apply-templates/> 
            </xsl:when>            
            <xsl:when test="scnp">
                <xsl:apply-templates/>
            </xsl:when>
            <xsl:otherwise>
                (unknown order)
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>
    
    <xsl:template match="dpTrans">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> Dependency Pair Transformation</h3>
        <xsl:choose>
        <xsl:when test="count(dps/rules/rule) &gt; 0">
          The following set of initial dependency pairs has been identified.
          <xsl:apply-templates select="dps/*"/>
          <xsl:apply-templates select="dpProof">
              <xsl:with-param name="indent" select="concat($indent, '.1')"/>
          </xsl:apply-templates>
        </xsl:when>
        <xsl:otherwise>
          The set of initial dependency pairs is empty, and hence the TRS is
          terminating. 
        </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="dpTrans" mode="nonterm">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> Dependency Pair Transformation</h3>
         The following set of initial dependency pairs has been identified.
         <xsl:apply-templates select="dps/rules"/>
        It remains to prove infiniteness of the resulting DP problem.
         <xsl:apply-templates select="dpNonterminationProof">
                <xsl:with-param name="indent" select="concat($indent, '.1')"/>
         </xsl:apply-templates>
    </xsl:template>
    
    
    
    <xsl:template match="unlabProc">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> Unlabeling Processor</h3>
        After removing one layer of labels
        we obtain the set of pairs
        <xsl:apply-templates select="dps/*"/>
        and the set of rules        
        <xsl:apply-templates select="trs/*"/>
        <p>
            <xsl:apply-templates select="*[3]">
                <xsl:with-param name="indent" select="concat($indent, '.1')"/>
            </xsl:apply-templates>
        </p>
    </xsl:template>
    
    <xsl:template match="unraveling">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> Unraveling</h3>
        <p>To prove that the CTRS is quasi reductive, we show termination of the following 
            unravelled system.
        </p>
        <table align="center">
            <xsl:for-each select="unravelingInformation/unravelingEntry/rule">
                <tr>
                    <td align="right">
                        <xsl:apply-templates select="lhs"/>
                    </td>
                    <td align="center">
                        <xsl:value-of select="$arrow"/>
                    </td>
                    <td align="left">
                        <xsl:apply-templates select="rhs"/>
                    </td>                    
                </tr>
            </xsl:for-each>
        </table>        
        <p>
            <xsl:apply-templates select="trsTerminationProof">
                <xsl:with-param name="indent" select="concat($indent, '.1')"/>
            </xsl:apply-templates>
        </p>
    </xsl:template>
    
    
    <xsl:template match="uncurriedSymbols">
        <xsl:param name="an" select="2"/>
        <p>
        <table align="center">
            <xsl:for-each select="uncurriedSymbolEntry">
                <xsl:variable name="n" select="arity"/>
                <tr><td><xsl:apply-templates select="*[1]"></xsl:apply-templates><xsl:call-template name="genVars">
                    <xsl:with-param name="n" select="$n"/>                    
                </xsl:call-template></td>
                    <td> is mapped to </td>
                    <xsl:for-each select="*">
                        <xsl:if test="position() > 2">
                            <td>
                                <xsl:apply-templates select="."></xsl:apply-templates><xsl:call-template name="genVars">
                                    <xsl:with-param name="n" select="$n + ($an - 1) * (position() - 3)"/>                    
                                </xsl:call-template><xsl:if test="position() != last()">, </xsl:if>
                            </td>
                        </xsl:if>
                    </xsl:for-each>
                </tr>
            </xsl:for-each>
        </table>
        </p>
    </xsl:template>
    
    <xsl:template match="uncurryInformation">
        <xsl:param name="an" select="2"/>
        <xsl:apply-templates select="*[1]"/>
        in combination with the following symbol map which also determines the applicative arities of these symbols.  
        <xsl:apply-templates select="uncurriedSymbols">
            <xsl:with-param name="an" select="$an"/>
        </xsl:apply-templates>        
        <br/>
        <xsl:choose>
            <xsl:when test="uncurryRules/rules/rule">
                The uncurry rules are:
                <xsl:apply-templates select="uncurryRules/*"/>                
            </xsl:when>
            <xsl:otherwise>
                There are no uncurry rules.
            </xsl:otherwise>
        </xsl:choose>
        <br/>
        <xsl:choose>
            <xsl:when test="etaRules/rules/rule">
                For the eta-expansion the following rules are added.
                <xsl:apply-templates select="etaRules/*"/>                
            </xsl:when>
            <xsl:otherwise>
                No rules have to be added for the eta-expansion.
            </xsl:otherwise>
        </xsl:choose>
        
    </xsl:template>

    <xsl:template match="uncurryProc">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> Uncurrying Processor</h3>
        <p>We uncurry 
           <xsl:choose>
               <xsl:when test="applicativeTop">the tuple-symbol
                   <xsl:apply-templates select="uncurryInformation">
                       <xsl:with-param name="an" select="applicativeTop/text()"/>                                                 
                   </xsl:apply-templates>
               </xsl:when>
               <xsl:otherwise>the binary symbol 
                   <xsl:apply-templates select="uncurryInformation"/>
               </xsl:otherwise>
           </xsl:choose>            
        </p>
        
        Uncurrying the pairs and rules, and adding the uncurrying rules yields the pair(s)
        <xsl:apply-templates select="dps/*"/>
        and the set of rules        
        <xsl:apply-templates select="trs/*"/>
        <p>
            <xsl:apply-templates select="dpProof">
                <xsl:with-param name="indent" select="concat($indent, '.1')"/>
            </xsl:apply-templates>
        </p>
    </xsl:template>

    <xsl:template match="uncurry">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> Uncurrying</h3>
        <p>We uncurry the binary symbol  
            <xsl:apply-templates select="uncurryInformation"/>
        </p>
        
        Uncurrying the rules and adding the uncurrying rules yields the new set of rules
        <xsl:apply-templates select="trs/*"/>
        <p>
            <xsl:apply-templates select="trsTerminationProof">
                <xsl:with-param name="indent" select="concat($indent, '.1')"/>
            </xsl:apply-templates>
        </p>
    </xsl:template>

    <xsl:template match="uncurry" mode="nonterm">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> Uncurrying</h3>
        <p>We uncurry the binary symbol  
            <xsl:apply-templates select="uncurryInformation"/>
        </p>
        
        Uncurrying the rules and adding the uncurrying rules yields the new set of rules
        <xsl:apply-templates select="trs/*"/>
        <p>
            <xsl:apply-templates select="trsNonterminationProof">
                <xsl:with-param name="indent" select="concat($indent, '.1')"/>
            </xsl:apply-templates>
        </p>
    </xsl:template>
    
    <xsl:template match="uncurry" mode="relative">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> Uncurrying</h3>
        <p>We uncurry the binary symbol  
            <xsl:apply-templates select="uncurryInformation"/>
        </p>
        
        Uncurrying the rules and adding the uncurrying rules yields the new set of rules
        <xsl:apply-templates select="*[2]/*"/>
        and
        <xsl:apply-templates select="*[3]/*"/>        
        <p>
            <xsl:apply-templates select="relativeTerminationProof">
                <xsl:with-param name="indent" select="concat($indent, '.1')"/>
            </xsl:apply-templates>
        </p>
    </xsl:template>
    
    <xsl:template match="permutingArgumentFilter">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> Permuting Argument Filter</h3>
        <p>We permute some arguments by the following argument filter.  
            <xsl:apply-templates select="argumentFilter"/>
        </p>
        
        Afterwards termination of the resulting TRS is proven.
        <p>
            <xsl:apply-templates select="trsTerminationProof">
                <xsl:with-param name="indent" select="concat($indent, '.1')"/>
            </xsl:apply-templates>
        </p>
    </xsl:template>    

    <xsl:template match="permutingArgumentFilter" mode="relative">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> Permuting Argument Filter</h3>
        <p>We permute some arguments by the following argument filter.  
            <xsl:apply-templates select="argumentFilter"/>
        </p>
        
        Afterwards relative termination of the resulting TRSs is proven.
        <p>
            <xsl:apply-templates select="relativeTerminationProof">
                <xsl:with-param name="indent" select="concat($indent, '.1')"/>
            </xsl:apply-templates>
        </p>
    </xsl:template>    
    
    
    <xsl:template match="split">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> Split</h3>
        <p>We split R in the relative problem D/R-D and R-D, where the rules D
            <xsl:apply-templates select="trs/*"/>
            are deleted.
        </p>
        <p>
            <xsl:apply-templates select="trsTerminationProof[1]">
                <xsl:with-param name="indent" select="concat($indent, '.1')"/>
            </xsl:apply-templates>
        </p>
        <p>
            <xsl:apply-templates select="trsTerminationProof[2]">
                <xsl:with-param name="indent" select="concat($indent, '.2')"/>
            </xsl:apply-templates>
        </p>
    </xsl:template>
    
    <xsl:template match="splitProc">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> Split</h3>
        <p>We split (P,R) into the relative DP-problem (PD,P-PD,RD,R-RD) and (P-PD,R-RD) where the pairs PD
            <xsl:apply-templates select="dps/*"/>
            and the rules RD
            <xsl:apply-templates select="trs/*"/>            
            are deleted.
        </p>
        <p>
            <xsl:apply-templates select="dpProof[1]">
                <xsl:with-param name="indent" select="concat($indent, '.1')"/>
            </xsl:apply-templates>
        </p>
        <p>
            <xsl:apply-templates select="dpProof[2]">
                <xsl:with-param name="indent" select="concat($indent, '.2')"/>
            </xsl:apply-templates>
        </p>
    </xsl:template>
    
    
    <xsl:template match="state">
        <xsl:apply-templates/>
    </xsl:template>
    
    <xsl:template match="treeAutomaton">
        <ul>
            <li><p>final states:</p>
                <p><xsl:text>{</xsl:text>
                    <xsl:for-each select="finalStates/*">
                        <xsl:if test="position() != 1">, </xsl:if>
                        <xsl:apply-templates select="."/>
                    </xsl:for-each>
                    <xsl:text>}</xsl:text>
                </p>
            </li>
            <li>
                <p>transitions:</p>
                <p>
                    <table>
                        <xsl:for-each select="transitions/transition">
                            <tr>
                                <td align="right">
                                    <xsl:apply-templates select="lhs/*[1]"/>
                                    <xsl:if test="lhs/height">
                                        <sub><xsl:apply-templates select="lhs/height"/></sub>                                        
                                    </xsl:if>
                                    <xsl:if test="count(lhs/*) != 1">
                                        <xsl:for-each select="lhs/state">
                                            <xsl:choose>
                                                <xsl:when test="position()=1">(</xsl:when>
                                                <xsl:otherwise>,</xsl:otherwise>
                                            </xsl:choose>
                                            <xsl:apply-templates select="."/>                                        
                                            <xsl:if test="position()=last()">)</xsl:if>
                                    </xsl:for-each>
                                    </xsl:if>
                                </td>
                                <td><xsl:value-of select="$rewrite"/></td>
                                <td align="left"><xsl:apply-templates select="rhs/state"/></td>
                            </tr>
                        </xsl:for-each>
                    </table>
                </p>
            </li>
        </ul>
    </xsl:template>
    
    <xsl:template match="removeNonApplicableRules">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> Removal of non-applicable rules</h3>
        The following rules have arguments which are not in normal form. Due to the strategy restrictions these can be removed.                
        <xsl:apply-templates select="trs/*"/>
        <p>
            <xsl:apply-templates select="trsTerminationProof">
                <xsl:with-param name="indent" select="concat($indent, '.1')"/>
            </xsl:apply-templates>
        </p>
    </xsl:template>

    <xsl:template match="removeNonApplicableRules" mode="complexity">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> Removal of non-applicable rules</h3>
        The following rules have arguments which are not in normal form. Due to the strategy restrictions these can be removed.                
        <xsl:apply-templates select="trs/*"/>
        <p>
            <xsl:apply-templates select="complexityProof">
                <xsl:with-param name="indent" select="concat($indent, '.1')"/>
            </xsl:apply-templates>            
        </p>
    </xsl:template>        
    
    
    <xsl:template match="bounds">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> Bounds</h3>
        The given TRS is 
        <xsl:choose>
            <xsl:when test="type/roof">roof</xsl:when>
            <xsl:when test="type/match">match</xsl:when>
        </xsl:choose>
        <xsl:text>-(raise)-bounded by </xsl:text>
        <xsl:apply-templates select="bound"/>.
        This is shown by the following automaton.
        <xsl:call-template name="compatibleTreeAutomaton"/>
    </xsl:template>
    
    <xsl:template match="unlab">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> Unlabeling</h3>
        After removing one layer of labels
        we obtain the TRS  
        <xsl:apply-templates select="trs/*"/>
        <p>
            <xsl:apply-templates select="trsTerminationProof">
                <xsl:with-param name="indent" select="concat($indent, '.1')"/>
            </xsl:apply-templates>
        </p>
    </xsl:template>

    <xsl:template match="unlab" mode="relative">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> Unlabeling</h3>
        After removing one layer of labels
        we obtain the TRSs R:       
        <xsl:apply-templates select="*[1]/*"/>
        and S:
        <xsl:apply-templates select="*[2]/*"/>
        <p>
            <xsl:apply-templates select="relativeTerminationProof">
                <xsl:with-param name="indent" select="concat($indent, '.1')"/>
            </xsl:apply-templates>
        </p>
    </xsl:template>

    <xsl:template match="constantToUnary">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> Constant to Unary</h3>
        Every constant is turned into a unary function symbol to obtain the TRS        
        <xsl:apply-templates select="trs/*"/>
        <p>
            <xsl:apply-templates select="trsTerminationProof">
                <xsl:with-param name="indent" select="concat($indent, '.1')"/>
            </xsl:apply-templates>
        </p>
    </xsl:template>
    
    <xsl:template match="constantToUnary" mode="nonterm">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> Constant to Unary</h3>
        Every constant is turned into a unary function symbol to obtain the TRS        
        <xsl:apply-templates select="trs/*"/>
        <p>
            <xsl:apply-templates select="trsNonterminationProof">
                <xsl:with-param name="indent" select="concat($indent, '.1')"/>
            </xsl:apply-templates>
        </p>
    </xsl:template>
    
    <xsl:template match="stringReversal" mode="nonterm">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> String Reversal</h3>
        Since only unary symbols occur, one can reverse all terms and obtains the TRS        
        <xsl:apply-templates select="trs/*"/>
        <p>
            <xsl:apply-templates select="trsNonterminationProof">
                <xsl:with-param name="indent" select="concat($indent, '.1')"/>
            </xsl:apply-templates>
        </p>
    </xsl:template>
    
    <xsl:template match="stringReversal" mode="relNonterm">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> String Reversal</h3>
        Since only unary symbols occur, one can reverse all terms and obtains the TRSs        
        <xsl:apply-templates select="trs[1]/*"/>
        and 
        <xsl:apply-templates select="trs[2]/*"/>
        <p>
            <xsl:apply-templates select="relativeNonterminationProof">
                <xsl:with-param name="indent" select="concat($indent, '.1')"/>
            </xsl:apply-templates>
        </p>
    </xsl:template>    
    
    
    <xsl:template match="stringReversal">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> String Reversal</h3>
        Since only unary symbols occur, one can reverse all terms and obtains the TRS        
        <xsl:apply-templates select="trs/*"/>
        <p>
            <xsl:apply-templates select="trsTerminationProof">
                <xsl:with-param name="indent" select="concat($indent, '.1')"/>
            </xsl:apply-templates>
        </p>
    </xsl:template>

    <xsl:template match="stringReversal" mode="relative">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> String Reversal</h3>
        Since only unary symbols occur, one can reverse all terms and obtains the TRS  R:
        <xsl:apply-templates select="*[1]/*"/>
        and S:
        <xsl:apply-templates select="*[2]/*"/>
        <p>
            <xsl:apply-templates select="relativeTerminationProof">
                <xsl:with-param name="indent" select="concat($indent, '.1')"/>
            </xsl:apply-templates>
        </p>
    </xsl:template>
    
    <xsl:template match="semlab">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> Semantic Labeling</h3>
        <xsl:apply-templates select="model"/>
        We obtain the labeled TRS
        <xsl:apply-templates select="trs/*"/>
        <xsl:apply-templates select="innermostLhss" mode="optional"/>
        <p>
            <xsl:apply-templates select="trsTerminationProof">
                <xsl:with-param name="indent" select="concat($indent, '.1')"/>
            </xsl:apply-templates>
        </p>
    </xsl:template>

    <xsl:template match="semlab" mode="relative">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> Semantic Labeling</h3>
        <xsl:apply-templates select="model"/>
        We obtain the labeled TRS R:
        <xsl:apply-templates select="*[2]/*"/>
        and S:
        <xsl:apply-templates select="*[3]/*"/>
        <xsl:apply-templates select="innermostLhss" mode="optional"/>
        <p>
            <xsl:apply-templates select="relativeTerminationProof">
                <xsl:with-param name="indent" select="concat($indent, '.1')"/>
            </xsl:apply-templates>
        </p>
    </xsl:template>
    
    <xsl:template match="flatContexts">
      <p>
      {<xsl:for-each select="*">
        <xsl:apply-templates select="."/>
        <xsl:if test="last() != position()">, </xsl:if>
      </xsl:for-each>}
      </p>
    </xsl:template>

    <xsl:template match="flatContextClosure">
      <xsl:param name="indent"/>
      <h3><xsl:value-of select="$indent"/> Closure Under Flat Contexts</h3>
      Using the flat contexts
      <xsl:apply-templates select="flatContexts"/>
      We obtain the transformed TRS
      <xsl:apply-templates select="trs/*"/>
      <p>
        <xsl:apply-templates select="*[3]">
          <xsl:with-param name="indent" select="concat($indent,'.1')"/>
        </xsl:apply-templates>
      </p>
    </xsl:template>

    <xsl:template match="flatContextClosure" mode="relative">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> Closure Under Flat Contexts</h3>
        Using the flat contexts
        <xsl:apply-templates select="flatContexts"/>
        We obtain the transformed TRSs
        <xsl:apply-templates select="*[2]"/>
        and 
        <xsl:apply-templates select="*[3]"/>
        <p>
            <xsl:apply-templates select="relativeTerminationProof">
                <xsl:with-param name="indent" select="concat($indent, '.1')"/>
            </xsl:apply-templates>
        </p>
    </xsl:template>
    
    <xsl:template match="freshSymbol">
      <xsl:apply-templates/>
    </xsl:template>

    <xsl:template match="flatContextClosureProc">
      <xsl:param name="indent"/>
      <h3><xsl:value-of select="$indent"/> Closure Under Flat Contexts</h3>
      Using
      <!--
      the fresh function symbol
      <xsl:apply-templates select="freshSymbol"/>
      and
      -->
      the flat contexts
      <xsl:apply-templates select="flatContexts"/>
      We obtain the set of pairs
      <xsl:apply-templates select="dps/*"/>
      and the rules:
      <xsl:apply-templates select="trs/*"/>
      <p>
        <xsl:apply-templates select="*[5]">
          <xsl:with-param name="indent" select="concat($indent, '.1')"/>
        </xsl:apply-templates>
      </p>
    </xsl:template>
    
    <xsl:template match="semlabProc">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> Semantic Labeling Processor</h3>
        <xsl:apply-templates select="model"/>
        We obtain the set of labeled pairs
        <xsl:apply-templates select="dps/*"/>
        and the set of labeled rules:        
        <xsl:apply-templates select="trs/*"/>
        <xsl:apply-templates select="innermostLhss" mode="optional"/>
        <p>
            <xsl:apply-templates select="dpProof">
                <xsl:with-param name="indent" select="concat($indent, '.1')"/>
            </xsl:apply-templates>
        </p>
    </xsl:template>

    <xsl:template match="usableRulesProc">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> Usable Rules Processor</h3>
        <p>We restrict the rewrite rules to the following usable rules of the DP problem.</p> 
        <xsl:apply-templates select="usableRules/*"/>        
            <xsl:apply-templates select="dpProof">
                <xsl:with-param name="indent" select="concat($indent, '.1')"/>
            </xsl:apply-templates>
        
    </xsl:template>
    
    <xsl:template match="innermostLhssRemovalProc">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> Innermost Lhss Removal Processor</h3>
        <p>We restrict the innermost strategy to the following left hand sides.</p>
        <xsl:apply-templates select="innermostLhss"/>        
            <xsl:apply-templates select="dpProof">
                <xsl:with-param name="indent" select="concat($indent, '.1')"/>
            </xsl:apply-templates>        
    </xsl:template>
    
    <xsl:template mode="dpNonterm" match="innermostLhssRemovalProc">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> Innermost Lhss Removal Processor</h3>
        <p>We restrict the innermost strategy to the following left hand sides.</p>
        <xsl:apply-templates select="innermostLhss"/>        
            <xsl:apply-templates select="dpNonterminationProof">
                <xsl:with-param name="indent" select="concat($indent, '.1')"/>
            </xsl:apply-templates>        
    </xsl:template>

    <xsl:template mode="dpNonterm" match="innermostLhssIncreaseProc">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> Innermost Lhss Increase Processor</h3>
        We add the following left hand sides to the innermost strategy.
        <xsl:apply-templates select="innermostLhss"/>
        <p>
            <xsl:apply-templates select="dpNonterminationProof">
                <xsl:with-param name="indent" select="concat($indent, '.1')"/>
            </xsl:apply-templates>
        </p>
    </xsl:template>

    <xsl:template mode="dpNonterm" match="switchFullStrategyProc">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> Full Strategy Switch Processor</h3>
        We have a locally confluent overlay TRS, no overlaps between P and R,
        and the strategy is less than innermost. Hence, it suffices to prove non-termination for the
        full rewrite relation.
        <xsl:apply-templates select="wcrProof"/>
        <p>
            <xsl:apply-templates select="dpNonterminationProof">
                <xsl:with-param name="indent" select="concat($indent, '.1')"/>
            </xsl:apply-templates>
        </p>
    </xsl:template>

    <xsl:template mode="nonterm" match="switchFullStrategy">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> Full Strategy Switch</h3>
        We have a locally confluent overlay TRS
        and the strategy is less than innermost. Hence, it suffices to prove non-termination for the
        full rewrite relation.
        <xsl:apply-templates select="wcrProof"/>
        <p>
            <xsl:apply-templates select="trsNonterminationProof">
                <xsl:with-param name="indent" select="concat($indent, '.1')"/>
            </xsl:apply-templates>
        </p>
    </xsl:template>
    
    <xsl:template mode="nonterm" match="innermostLhssIncrease">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> Innermost Lhss Increase</h3>
        We add the following left hand sides to the innermost strategy.
        <xsl:apply-templates select="innermostLhss"/>
        <p>
            <xsl:apply-templates select="trsNonterminationProof">
                <xsl:with-param name="indent" select="concat($indent, '.1')"/>
            </xsl:apply-templates>
        </p>
    </xsl:template>    
    
    
    <xsl:template match="rewritingProc">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> Rewriting Processor</h3>
        We rewrite the right hand side of the pair  
        <xsl:apply-templates select="rule[1]" mode="centered"/>
        resulting in 
        <table align="center">            
            <tr>
                <td><xsl:apply-templates select="rule[last()]/lhs"/></td>
                <td><xsl:value-of select="$rewrite"/></td>
                <td><xsl:apply-templates select="rule[last()]/rhs"/></td>                        
            </tr>
        </table>        
        <p>
            <xsl:apply-templates select="dpProof">
                <xsl:with-param name="indent" select="concat($indent, '.1')"/>
            </xsl:apply-templates>
        </p>
    </xsl:template>
    
    <xsl:template match="rewritingProc" mode="dpNonterm">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> Rewriting Processor</h3>
        We rewrite the right hand side of the pair  
        <xsl:apply-templates select="rule[1]" mode="centered"/>
        resulting in 
        <table align="center">            
            <tr>
                <td><xsl:apply-templates select="rule[last()]/lhs"/></td>
                <td><xsl:value-of select="$rewrite"/></td>
                <td><xsl:apply-templates select="rule[last()]/rhs"/></td>                        
            </tr>
        </table>        
        <p>
            <xsl:apply-templates select="dpNonterminationProof">
                <xsl:with-param name="indent" select="concat($indent, '.1')"/>
            </xsl:apply-templates>
        </p>
    </xsl:template>
    
    
    <xsl:template match="instantiationProc">
        <xsl:param name="indent"/>
        <xsl:call-template name="instantiationProc">
            <xsl:with-param name="indent" select="$indent"/>
        </xsl:call-template>
    </xsl:template>

    <xsl:template match="forwardInstantiationProc">
        <xsl:param name="indent"/>
        <xsl:call-template name="instantiationProc">
            <xsl:with-param name="indent" select="$indent"/>
            <xsl:with-param name="prefix">Forward</xsl:with-param>
        </xsl:call-template>        
    </xsl:template>
    
    <xsl:template name="instantiationProc">
        <xsl:param name="indent"/>
        <xsl:param name="prefix"/>
        <h3><xsl:value-of select="$indent"/><xsl:value-of select="concat(' ',$prefix)"/> Instantiation Processor</h3>
        We instantiate the pair  
        <xsl:apply-templates select="rule" mode="centered"/>
        to the following set of pairs
        <xsl:apply-templates select="instantiations/rules"/>
        <p>
            <xsl:apply-templates select="dpProof">
                <xsl:with-param name="indent" select="concat($indent, '.1')"/>
            </xsl:apply-templates>
        </p>
    </xsl:template>

    <xsl:template match="instantiationProc" mode="dpNonterm">
        <xsl:param name="indent"/>
        <xsl:param name="prefix"/>
        <h3><xsl:value-of select="$indent"/><xsl:value-of select="concat(' ',$prefix)"/> Instantiation Processor</h3>
        The pairs are instantiated to the following pairs.
        <xsl:apply-templates select="dps/rules"/>
        <p>
            <xsl:apply-templates select="dpNonterminationProof">
                <xsl:with-param name="indent" select="concat($indent, '.1')"/>
            </xsl:apply-templates>
        </p>
    </xsl:template>
    
    <xsl:template match="narrowingProc">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> Narrowing Processor</h3>
        We consider all narrowings of the pair 
        <xsl:apply-templates select="rule" mode="centered"/>
        below position 
        <xsl:apply-templates select="positionInTerm"/>
        to get the following set of pairs
        <xsl:apply-templates select="narrowings/rules"/>
        <p>
            <xsl:apply-templates select="dpProof">
                <xsl:with-param name="indent" select="concat($indent, '.1')"/>
            </xsl:apply-templates>
        </p>
    </xsl:template>
    
    <xsl:template match="narrowingProc" mode="dpNonterm">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> Narrowing Processor</h3>
        We consider narrowings of the pair 
        <xsl:apply-templates select="rule" mode="centered"/>
        below position 
        <xsl:apply-templates select="positionInTerm"/>
        to get the following set of pairs
        <xsl:apply-templates select="narrowings/rules"/>
        <p>
            <xsl:apply-templates select="dpNonterminationProof">
                <xsl:with-param name="indent" select="concat($indent, '.1')"/>
            </xsl:apply-templates>
        </p>
    </xsl:template>
    
    
    <xsl:template match="complexConstantRemovalProc">
        <xsl:param name="indent"/>
        <xsl:param name="prefix"/>
        <h3><xsl:value-of select="$indent"/><xsl:value-of select="concat(' ',$prefix)"/> Complex Constant Removal Processor</h3>
        We replace the term 
        <xsl:apply-templates select="*[1]"/> by a fresh variable. This results in the following new pairs.
        <p>
            <table>
                <xsl:attribute name="align">center</xsl:attribute>
                <xsl:for-each select="ruleMap/ruleMapEntry">
                    <tr>
                        <td align="right">
                            <xsl:apply-templates select="rule[2]/lhs"/>
                        </td>
                        <td align="center">
                            <xsl:value-of select="$arrow"/>
                        </td>
                        <td align="left">
                            <xsl:apply-templates select="rule[2]/rhs"/>
                        </td>
                    </tr>
                </xsl:for-each>
            </table>            
        </p>
        <p>
            <xsl:apply-templates select="dpProof">
                <xsl:with-param name="indent" select="concat($indent, '.1')"/>
            </xsl:apply-templates>
        </p>
    </xsl:template>
    
    <xsl:template match="arithFunction">
        <xsl:apply-templates select="*[1]" mode="arithFun"/>
    </xsl:template>
    
    <xsl:template mode="arithFun" match="number">
        <xsl:apply-templates/>
    </xsl:template>

    <xsl:template mode="arithFun" match="variable">
        <span class="var">x<sub><xsl:apply-templates/></sub></span>
    </xsl:template>

    <xsl:template mode="arithFun" match="sum">
        <xsl:for-each select="*">
            <xsl:apply-templates select="."/>
            <xsl:if test="position() != last()"> + </xsl:if>
        </xsl:for-each>
    </xsl:template>

    <xsl:template mode="arithFun" match="prod">
        <xsl:text>(</xsl:text>
        <xsl:for-each select="*">
            <xsl:apply-templates select="."/>
            <xsl:if test="position() != last()"> * </xsl:if>
        </xsl:for-each>
        <xsl:text>)</xsl:text>        
    </xsl:template>
    
    <xsl:template mode="arithFun" match="min">
        <xsl:text>min(</xsl:text>
        <xsl:for-each select="*">
            <xsl:apply-templates select="."/>
            <xsl:if test="position() != last()">,</xsl:if>
        </xsl:for-each>
        <xsl:text>)</xsl:text>
    </xsl:template>

    <xsl:template mode="arithFun" match="max">
        <xsl:text>max(</xsl:text>
        <xsl:for-each select="*">
            <xsl:apply-templates select="."/>
            <xsl:if test="position() != last()">,</xsl:if>
        </xsl:for-each>
        <xsl:text>)</xsl:text>
    </xsl:template>

    <xsl:template mode="arithFun" match="ifEqual">
        <xsl:text>(if </xsl:text>
        <xsl:apply-templates select="*[1]"/>
        <xsl:text> = </xsl:text>
        <xsl:apply-templates select="*[2]"/>
        <xsl:text> then </xsl:text>
        <xsl:apply-templates select="*[3]"/>
        <xsl:text> else </xsl:text>
        <xsl:apply-templates select="*[4]"/>
        <xsl:text>)</xsl:text>
    </xsl:template>
    
    <xsl:template match="model">
        <xsl:apply-templates/>
    </xsl:template>
    
    <xsl:template match="rootLabeling">
        <p>
            <xsl:text>Root-labeling is applied</xsl:text>
            <xsl:if test="count(*) = 1">
                <xsl:text> with a special treatment of the blocking symbol </xsl:text>
                <xsl:apply-templates/>
            </xsl:if>
            <xsl:text>.</xsl:text>
        </p>
    </xsl:template>
    
    <xsl:template match="finiteModel">
        The following interpretations form a 
        <xsl:choose>
            <xsl:when test="tupleOrder">
                quasi-model 
            </xsl:when>
            <xsl:otherwise>
                model
            </xsl:otherwise>
        </xsl:choose>
            of the rules.
            <xsl:if test="tupleOrder">
                (Here, the order is 
                <xsl:choose>
                    <xsl:when test="tupleOrder/pointWise">
                        the pointwise extension of > to vectors over natural numbers)
                    </xsl:when>
                </xsl:choose>                
            </xsl:if>
        <p>
          As carrier we take the set 
          <xsl:choose>
              <xsl:when test="carrierSize/text() = 1">{0}</xsl:when>
              <xsl:when test="carrierSize/text() = 2">{0,1}</xsl:when>
              <xsl:when test="carrierSize/text() = 3">{0,1,2}</xsl:when>
              <xsl:otherwise>{0,...,<xsl:value-of select="carrierSize/text() - 1"/>}</xsl:otherwise>
          </xsl:choose>
          <xsl:text>.</xsl:text>
	  Symbols are labeled by the interpretation of their arguments using the interpretations
          (modulo <xsl:value-of select="carrierSize/text()"/>):
          <p>
              <table>
                  <xsl:attribute name="align">center</xsl:attribute>
                  <xsl:for-each select="interpret">
                      <tr>
                          <td><xsl:attribute name="align">right</xsl:attribute>
                              <xsl:text>[</xsl:text><xsl:apply-templates select="*[1]"/><xsl:call-template name="genVars">
                                  <xsl:with-param name="n" select="arity"/>
                              </xsl:call-template><xsl:text>]</xsl:text>
                          </td>
                          <td><xsl:attribute name="align">center</xsl:attribute> = </td>
                          <td><xsl:attribute name="align">left</xsl:attribute><xsl:apply-templates select="arithFunction"/></td>
                      </tr>           
                  </xsl:for-each>
                  <!--
                  <tr>
                      <td><xsl:attribute name="align">right</xsl:attribute>
                          <xsl:text>[f(</xsl:text><span class="var">x<sub>1</sub></span>,...,<span class="var">x<sub>n</sub></span>)]
                      </td>
                      <td><xsl:attribute name="align">center</xsl:attribute> = </td>
                      <td><xsl:attribute name="align">left</xsl:attribute>
                          0 
                      </td>
                      <td><xsl:attribute name="align">right</xsl:attribute>
                          for all other symbols f of arity n
                      </td>                            
                  </tr>
                  -->
              </table>          
              </p>
        </p>
    </xsl:template>
    
    <xsl:template name="ProofStep">
      <xsl:param name="indent"/>
      <xsl:param name="name"/>
      <xsl:param name="justification"/>
      <xsl:param name="pairs"/>
      <xsl:param name="urules">null</xsl:param>
      <xsl:param name="rules">null</xsl:param>
      <xsl:param name="proof"/>
        Using the <xsl:value-of select="$name"/>
      <xsl:apply-templates select="$justification"/>
      <xsl:if test="string($urules) != 'null'">
        <xsl:choose>
          <xsl:when test="count($urules) &gt; 0">
            together with the usable
            rule<xsl:if test="count($urules) &gt; 1">s</xsl:if>
            <xsl:apply-templates select="$urules/.."/>
            (w.r.t. the implicit argument filter of the reduction pair),
          </xsl:when>
          <xsl:otherwise>
            having no usable rules (w.r.t. the implicit argument filter of the
            reduction pair),
          </xsl:otherwise>
        </xsl:choose>
      </xsl:if>
      <xsl:choose>
        <xsl:when test="count($pairs) &gt; 0">
          the
          pair<xsl:if test="count($pairs) &gt; 1">s</xsl:if> 
          <xsl:apply-templates select="$pairs/.."/>
	  <xsl:if test="string($rules) != 'null'">
	    and
            <xsl:choose>
              <xsl:when test="count($rules) &gt; 0">
                the
                rule<xsl:if test="count($rules) &gt; 1">s</xsl:if>
                <xsl:apply-templates select="$rules/.."/>
              </xsl:when>
              <xsl:otherwise>
                no rules
              </xsl:otherwise>
            </xsl:choose>
	  </xsl:if>
          remain<xsl:if test="count($pairs) = 1 and string($rules) = 'null'">s</xsl:if>.
        </xsl:when>
        <xsl:otherwise>
	  <xsl:choose>
	    <xsl:when test="string($rules) != 'null' and count($rules) &gt; 0">
              all pairs could be removed, but the
              rule<xsl:if test="count($rules) &gt; 1">s</xsl:if>
              <xsl:apply-templates select="$rules/.."/>
              remain<xsl:if test="count($rules) &gt; 1">s</xsl:if>.
	    </xsl:when>
	    <xsl:when test="string($rules) != 'null'">
              all pairs and rules could be removed.
	    </xsl:when>
	    <xsl:otherwise>
	      all pairs could be removed.
	    </xsl:otherwise>
	  </xsl:choose>
        </xsl:otherwise>
      </xsl:choose>
      
        <xsl:apply-templates select="$proof">
          <xsl:with-param name="indent" select="concat($indent,'.1')"/>
        </xsl:apply-templates>
      
    </xsl:template>
    
    <xsl:template match="argumentFilter">
          <p>
          <table>
              <xsl:for-each select="argumentFilterEntry">
          <tr>
            <td align="right"><xsl:value-of select="$pi"/>(<xsl:apply-templates select="*[1]"/>)</td>
            <td align="center">=</td>
            <td align="left">
                <xsl:choose>
                    <xsl:when test="collapsing">
                        <xsl:value-of select="collapsing"/>                        
                    </xsl:when>
                    <xsl:when test="nonCollapsing">
                        <xsl:text>[</xsl:text>
                        <xsl:for-each select="nonCollapsing/position">
                            <xsl:apply-templates/>
                            <xsl:if test="position() != last()">
                                <xsl:text>,</xsl:text>
                            </xsl:if>
                        </xsl:for-each>
                        <xsl:text>]</xsl:text>
                    </xsl:when>
                    <xsl:otherwise>
                        <xsl:message terminate="yes">unknown argument filter entry</xsl:message>                        
                    </xsl:otherwise>                    
                </xsl:choose>                
            </td>
          </tr>
          </xsl:for-each>
          </table>
          </p>
    </xsl:template>

    <xsl:template match="projectedRewriteSequence">
      <li>
      the projected left-hand side of the rule
      <p>
      <xsl:apply-templates select="rule"/>
      </p>
      is rewritten according to
      <xsl:apply-templates select="rewriteSequence"/>
      </li>
    </xsl:template>
    
    <xsl:template match="subtermProc">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> Subterm Criterion Processor</h3>
        We use the projection
        <xsl:apply-templates select="argumentFilter"/>
        <xsl:choose>
          <xsl:when test="count(projectedRewriteSequence) &gt; 0">
            and the following rewrite sequences:
            <p>
            <ul>
            <xsl:apply-templates select="projectedRewriteSequence"/>
            </ul>
            </p>
          </xsl:when>
        </xsl:choose>
        <xsl:choose>
          <xsl:when test="count(dps/rules/*) &gt; 0">
            and remain with the pairs:
            <xsl:apply-templates select="dps/*"/>
          </xsl:when>
          <xsl:otherwise>
          to remove all pairs.
          </xsl:otherwise>
        </xsl:choose>
        <xsl:apply-templates select="dpProof">
          <xsl:with-param name="indent" select="concat($indent, '.1')"/>
        </xsl:apply-templates>
    </xsl:template>

    <xsl:template match="switchToTRS">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> Switch to TRS Processor</h3>
        We merge the DPs and rules into one TRS.
        <xsl:apply-templates select="trsTerminationProof">
            <xsl:with-param name="indent" select="concat($indent, '.1')"/>
        </xsl:apply-templates>
    </xsl:template>
    
    <xsl:template match="finitenessAssumption">
      <xsl:param name="indent"/>
      <h3><xsl:value-of select="$indent"/> Finiteness Assumption</h3>
      We assume finiteness of the DP problem (P,R) where P is
      <xsl:apply-templates select="./dpInput/dps/rules/*/.."/>
      and R is
      <xsl:choose>
        <xsl:when test="count(dpInput/trs/rules/rule) = 0">
          empty.
        </xsl:when>
        <xsl:otherwise>
          the following TRS.
          <xsl:apply-templates select="dpInput/trs/rules/*/.."/>
        </xsl:otherwise>
      </xsl:choose>        
    </xsl:template>

    <xsl:template match="infinitenessAssumption" mode="dpNonterm">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> Infiniteness Assumption</h3>
        We assume infiniteness of the DP problem (P,R) where P is
        <xsl:apply-templates select="./dpInput/dps/rules/*/.."/>
        and R is
        <xsl:choose>
            <xsl:when test="count(dpInput/trs/rules/rule) = 0">
                empty.
            </xsl:when>
            <xsl:otherwise>
                the following TRS.
                <xsl:apply-templates select="dpInput/trs/rules/*/.."/>
            </xsl:otherwise>
        </xsl:choose>
        <xsl:apply-templates select="dpInput/strategy"/>
    </xsl:template>
    
    <xsl:template match="unknownProof">
        <xsl:param name="indent"/>
        <xsl:call-template name="unknownProof">
            <xsl:with-param name="indent" select="$indent"/>
        </xsl:call-template>
    </xsl:template>

    <xsl:template match="unknownProof" mode="relative">
        <xsl:param name="indent"/>
        <xsl:call-template name="unknownProof">
            <xsl:with-param name="indent" select="$indent"/>
        </xsl:call-template>
    </xsl:template>

    <xsl:template match="unknownProof" mode="relNonterm">
        <xsl:param name="indent"/>
        <xsl:call-template name="unknownProof">
            <xsl:with-param name="indent" select="$indent"/>
        </xsl:call-template>
    </xsl:template>
    
    <xsl:template match="unknownProof" mode="nonterm">       
        <xsl:param name="indent"/>
        <xsl:call-template name="unknownProof">
            <xsl:with-param name="indent" select="$indent"/>
        </xsl:call-template>
    </xsl:template>
    
    <xsl:template match="unknownProof" mode="dpNonterm">       
        <xsl:param name="indent"/>
        <xsl:call-template name="unknownProof">
            <xsl:with-param name="indent" select="$indent"/>
        </xsl:call-template>
    </xsl:template>    
    
    <xsl:template name="unknownProof">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> Unknown Proof</h3>
        Using some unknown proof method called 
        <xsl:apply-templates select="description"/>
        one can switch to the following subproblems:
        <ul>
            <xsl:for-each select="subProof">
                <xsl:variable name="nr" select="position()"/>
                <li>
                    <xsl:apply-templates select="*[1]"/>
                    <xsl:apply-templates select="*[2]">
                        <xsl:with-param name="indent" select="concat($indent, concat('.', $nr))"/>
                    </xsl:apply-templates>
                </li>                    
            </xsl:for-each>
        </ul>
    </xsl:template>
    

    <xsl:template match="infinitenessAssumption">
      <xsl:param name="indent"/>
      <h3><xsl:value-of select="$indent"/> Infiniteness Assumption</h3>
      We assume infiniteness of the DP problem (P,R) where P is
      <xsl:apply-templates select="./dpInput/dps/rules/*/.."/>
      and R is
      <xsl:choose>
        <xsl:when test="count(dpInput/trs/rules/rule) = 0">
          empty.
        </xsl:when>
        <xsl:otherwise>
          the following TRS.
          <xsl:apply-templates select="dpInput/trs/rules/*/.."/>
        </xsl:otherwise>
      </xsl:choose>        
    </xsl:template>

    <xsl:template match="terminationAssumption">
      <xsl:param name="indent"/>
      <h3><xsl:value-of select="$indent"/> Termination Assumption</h3>
      We assume termination of the following TRS
      <xsl:apply-templates select="trsInput/trs/rules/*/.."/>
        <xsl:if test="trsInput/relativeRules/rules/*/..">
            <xsl:text>relative to the following TRS</xsl:text>
            <xsl:apply-templates select="trsInput/relativeRules/rules/*/.."/>
        </xsl:if>
        <xsl:apply-templates select="trsInput/strategy"/>
    </xsl:template>

    <xsl:template match="relativeTerminationAssumption" mode="relative">
      <xsl:param name="indent"/>
      <h3><xsl:value-of select="$indent"/> Termination Assumption</h3>
      We assume nontermination of R relative to S where R is
      <xsl:apply-templates select="trsInput/trs/rules/*/.."/>
      and S is
      <xsl:choose>
        <xsl:when test="count(trsInput/relativeRules/rules/rule) = 0">
          empty.
        </xsl:when>
        <xsl:otherwise>
          the following TRS.
          <xsl:apply-templates select="trsInput/relativeRules/rules/*/.."/>
        </xsl:otherwise>
      </xsl:choose>        
    </xsl:template>

    <xsl:template match="nonterminationAssumption" mode="nonterm">
      <xsl:param name="indent"/>
      <h3><xsl:value-of select="$indent"/> Nontermination Assumption</h3>
      We assume nontermination of the following TRS.
      <xsl:apply-templates select="trsInput/trs/rules/*/.."/>
      <xsl:choose>
        <xsl:when test="count(trsInput/relativeRules/rules/rule) = 0">
        </xsl:when>
        <xsl:otherwise>
        Together with the relative rules
        <xsl:apply-templates select="trsInput/relativeRules/rules/*/.."/>
        </xsl:otherwise>
      </xsl:choose>
        <xsl:apply-templates select="trsInput/strategy"/>
    </xsl:template>

    <xsl:template match="redPairProc">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> Reduction Pair Processor</h3>
	<xsl:call-template name="ProofStep">
	  <xsl:with-param name="indent" select="$indent"/>
	  <xsl:with-param name="justification" select="orderingConstraintProof"/>
	  <xsl:with-param name="pairs" select="dps/rules/*"/>
	  <xsl:with-param name="proof" select="dpProof"/>
	</xsl:call-template>
    </xsl:template>
    
    <xsl:template match="conditionalConstraint">
        <xsl:apply-templates mode="cc"/>
    </xsl:template>
    <xsl:template match="implication" mode="cc">
        <xsl:if test="count(*) != 1">(</xsl:if>
        <xsl:for-each select="*">
            <xsl:if test="position() != 1">
                <xsl:value-of select="$implication"/>
            </xsl:if>            
            <xsl:apply-templates select="."/>
        </xsl:for-each>
        <xsl:if test="count(*) != 1">)</xsl:if>
    </xsl:template>
    <xsl:template match="all" mode="cc">
        <xsl:value-of select="$forall"/>
        <xsl:apply-templates select="*[1]"/>
        .        
        <xsl:apply-templates select="*[2]"/>
    </xsl:template>
    <xsl:template match="constraint" mode="cc">
        <xsl:apply-templates select="*[1]"/>
        <xsl:choose>
            <xsl:when test="nonStrict"> <xsl:value-of select="$ge"/> </xsl:when>
            <xsl:when test="rewrite"> <xsl:value-of select="$arrow"/><sup>*</sup> </xsl:when>
            <xsl:when test="strict"> &gt; </xsl:when>            
        </xsl:choose>
        <xsl:apply-templates select="*[3]"/>
    </xsl:template>
    
    <xsl:template match="conditionalConstraintProof">        
        <xsl:apply-templates mode = "cc"/>
    </xsl:template>
    
    <xsl:template match="final" mode="cc">
        <li><p>This constraint is kept as final constraint.</p></li>
    </xsl:template>
    
    <xsl:template match="sameConstructor" mode="cc">
        <li>
            Applying Rule "Same Constructor" results in<br/>             
            <xsl:apply-templates select="*[2]"/>
        </li>
        <xsl:apply-templates select="conditionalConstraintProof"/>
    </xsl:template>

    <xsl:template match="variableEquation" mode="cc">
        <li>
            Applying Rule "Variable in Equation" allows to substitute
          <xsl:apply-templates select="*[1]"/> by <xsl:apply-templates select="*[2]"/> which results in <br/>           
          <xsl:apply-templates select="*[3]"/>          
        </li>
        <xsl:apply-templates select="conditionalConstraintProof"/>
    </xsl:template>
    
    <xsl:template match="deleteCondition" mode="cc">
        <li>
          Applying Rule "Delete Conditions" results in <br/>          
          <xsl:apply-templates select="*[1]"/>          
        </li>
        <xsl:apply-templates select="conditionalConstraintProof"/>
    </xsl:template>

    <xsl:template match="simplifyCondition" mode="cc">
        <li>
            Applying Rule "Simplify Conditions" results in
            <br/>
            <xsl:apply-templates select="*[3]"/>            
        </li>
        <xsl:apply-templates select="conditionalConstraintProof"/>
    </xsl:template>

    <xsl:template match="funargIntoVar" mode="cc">
        <li>
            Applying Rule "Introduce fresh variable" results in
            <br/>
            <xsl:apply-templates select="*[4]"/>            
        </li>
        <xsl:apply-templates select="conditionalConstraintProof"/>
    </xsl:template>
    
    <xsl:template match="differentConstructor" mode="cc">
        <li>Applying Rule "Different Constructors" allows to drop this constraint.</li>
    </xsl:template>
    
    <xsl:template match="induction" mode="cc">
        <li>
            Applying Rule "Induction" on <xsl:apply-templates select="conditionalConstraint"/> results in the following <xsl:value-of select="count(ruleConstraintProofs/ruleConstraintProof)"/> new constraints.
        <ol>
            <xsl:for-each select="ruleConstraintProofs/ruleConstraintProof">
                <li>
                    <xsl:apply-templates select="conditionalConstraint"/>
                    <ul>
                        <xsl:apply-templates select="conditionalConstraintProof"/>
                    </ul>
                </li>
            </xsl:for-each>
        </ol>
        </li>
    </xsl:template>
    
    <xsl:template match="generalRedPairProc">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> Reduction Pair Processor</h3>        
        We apply the generic reduction pair processor using the  
        <xsl:apply-templates select="orderingConstraintProof"/>        
        <p>The pair(s) </p>
        <xsl:apply-templates select="strict"/>
        <p>are strictly oriented and the pair(s)</p>
        <xsl:apply-templates select="bound"/>
        <p>are bounded w.r.t. the constant <xsl:apply-templates select="condRedPairProof/*[1]"/>.
        </p>
        <p>The following constraints are generated for the pairs.</p>
        <ul>
            <xsl:for-each select="condRedPairProof//final/../../conditionalConstraint[last()]">
                <li><xsl:apply-templates select = "."/></li>
            </xsl:for-each>
        </ul>
        
        <p>
        The details are shown below:</p>
        <ul>
            
           <xsl:for-each select="condRedPairProof/conditions/condition">
             <li>
                 For the chain 
                 <xsl:for-each select="dpSequence/rules/rule">
                     <xsl:if test="position() != 1">, </xsl:if>
                     <xsl:apply-templates select="."/>
                 </xsl:for-each>
                 we build the initial constraint
                 <p>
                 <xsl:apply-templates select="conditionalConstraint"/>
                 </p>
                 <p>which is simplified as follows.</p>
                 <ul>
                 <xsl:apply-templates select="conditionalConstraintProof"/>
                 </ul>
             </li>
           </xsl:for-each>
            
        </ul>        
        <p>
        <xsl:choose>
            <xsl:when test="*[6]">
                We get two subproofs, in the first the strict pairs are deleted, in the second the bounded pairs are deleted.
            </xsl:when>
            <xsl:otherwise>
                We remove those pairs which are strictly decreasing and bounded.
            </xsl:otherwise>
        </xsl:choose>
        </p>
        
            <xsl:apply-templates select="*[5]">
                <xsl:with-param name="indent" select="concat($indent,'.1')"/>
            </xsl:apply-templates>
            <xsl:apply-templates select="*[6]">
                <xsl:with-param name="indent" select="concat($indent,'.2')"/>
            </xsl:apply-templates>
            
        
    </xsl:template>
    

    <xsl:template match="redPairUrProc">
      <xsl:param name="indent"/>
      <h3><xsl:value-of select="$indent"/> Reduction Pair Processor with Usable Rules</h3>
      <xsl:call-template name="ProofStep">
        <xsl:with-param name="indent" select="$indent"/>
          <xsl:with-param name="justification" select="orderingConstraintProof"/>
        <xsl:with-param name="pairs" select="dps/rules/*"/>
        <xsl:with-param name="urules" select="usableRules/rules/*"/>
        <xsl:with-param name="proof" select="dpProof"/>
      </xsl:call-template>
    </xsl:template>
        
    <xsl:template match="monoRedPairProc">
      <xsl:param name="indent"/>
      <h3><xsl:value-of select="$indent"/> Monotonic Reduction Pair Processor</h3>
      <xsl:call-template name="ProofStep">
        <xsl:with-param name="indent" select="$indent"/>
          <xsl:with-param name="justification" select="orderingConstraintProof"/>
        <xsl:with-param name="pairs" select="dps/rules/*"/>
	<xsl:with-param name="rules" select="trs/rules/*"/>
        <xsl:with-param name="proof" select="dpProof"/>
      </xsl:call-template>
  </xsl:template>

    <xsl:template match="monoRedPairUrProc">
      <xsl:param name="indent"/>
      <h3><xsl:value-of select="$indent"/> Monotonic Reduction Pair Processor with Usable Rules</h3>
      <xsl:call-template name="ProofStep">
        <xsl:with-param name="indent" select="$indent"/>
          <xsl:with-param name="justification" select="orderingConstraintProof"/>
        <xsl:with-param name="pairs" select="dps/rules/*"/>
	<xsl:with-param name="urules" select="usableRules/rules/*"/>
	<xsl:with-param name="rules" select="trs/rules/*"/>
        <xsl:with-param name="proof" select="dpProof"/>
      </xsl:call-template>
    </xsl:template>
    
    <xsl:template match="pIsEmpty">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> P is empty </h3>
        <p>There are no pairs anymore.</p>
    </xsl:template>

    <xsl:template match="dpRuleRemoval" mode="dpNonterm">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> Pair and Rule Removal</h3>
        Some pairs and rules have been removed and it remains to prove infiniteness for the new DP problem (P,R) where P is
        <xsl:choose>
            <xsl:when test="dps">
                <xsl:apply-templates select="dps/rules/*/.."/>
            </xsl:when>
            <xsl:otherwise> unchanged </xsl:otherwise>
        </xsl:choose>        
        and R is
        <xsl:choose>
            <xsl:when test="trs">
                <xsl:choose>
                    <xsl:when test="count(trs/rules/rule) = 0">
                        empty.
                    </xsl:when>
                    <xsl:otherwise>
                        the following TRS.
                        <xsl:apply-templates select="trs/rules/*/.."/>
                    </xsl:otherwise>                    
                </xsl:choose>                
            </xsl:when>
            <xsl:otherwise>
                unchanged.
            </xsl:otherwise>
        </xsl:choose>        
        <p>
            <xsl:apply-templates select="dpNonterminationProof">
                <xsl:with-param name="indent" select="concat($indent, '.1')"/>
            </xsl:apply-templates>
        </p>
    </xsl:template>
    
    <xsl:template match="ruleRemoval" mode="nonterm">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> Rule Removal</h3>
        Some rules have been removed and it remains to disprove termination of the following TRS.
        <xsl:apply-templates select="trs/rules/*/.."/>
        <p>
            <xsl:apply-templates select="trsNonterminationProof">
                <xsl:with-param name="indent" select="concat($indent, '.1')"/>
            </xsl:apply-templates>
        </p>
    </xsl:template>

    <xsl:template match="ruleShifting" mode="complexity">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> Rule Shifting</h3>
        The rules
        <xsl:apply-templates select="trs"/>
        are strictly oriented by the following 
        <xsl:apply-templates select="orderingConstraintProof"/>
        which has the intended complexity.
        <p>
            <xsl:apply-templates select="complexityProof">
                <xsl:with-param name="indent" select="concat($indent, '.1')"/>
            </xsl:apply-templates>
        </p>        
    </xsl:template>
    
    <xsl:template match="switchInnermost">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> Switch to Innermost Termination</h3>
        <p>The TRS is overlay and locally confluent:</p>        
        
            <xsl:apply-templates select="wcrProof/*"/>
        
        <p>Hence, it suffices to show innermost termination in the following.</p>        
            <xsl:apply-templates select="trsTerminationProof">
                <xsl:with-param name="indent" select="concat($indent, '.1')"/>
            </xsl:apply-templates>
        
    </xsl:template>
    
    <xsl:template match="switchInnermostProc">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> Switch to Innermost Termination</h3>
        The TRS does not have overlaps with the pairs and is locally confluent:        
        <p>
            <xsl:apply-templates select="wcrProof/*"/>
        </p>
        Hence, it suffices to show innermost termination in the following.
        <p>
            <xsl:apply-templates select="dpProof">
                <xsl:with-param name="indent" select="concat($indent, '.1')"/>
            </xsl:apply-templates>
        </p>
    </xsl:template>
    

    <xsl:template match="ruleRemoval">
      <xsl:param name="indent"/>
      <h3><xsl:value-of select="$indent"/> Rule Removal</h3>
      Using the
      <xsl:apply-templates select="orderingConstraintProof"/>                
      <xsl:choose>
        <xsl:when test="count(trs/rules/*) &gt; 0">
          the
          rule<xsl:if test="count(trs/rules/*) &gt; 1">s</xsl:if> 
          <xsl:apply-templates select="trs/rules/*/.."/>
          remain<xsl:if test="count(trs/rules/*) = 1">s</xsl:if>.
        </xsl:when>
        <xsl:otherwise>
          all rules could be removed.
        </xsl:otherwise>
      </xsl:choose>
      <p>
        <xsl:apply-templates select="trsTerminationProof">
          <xsl:with-param name="indent" select="concat($indent, '.1')"/>
        </xsl:apply-templates>
      </p>
    </xsl:template>

    <xsl:template match="ruleRemoval" mode="relative">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> Rule Removal</h3>
        Using the
        <xsl:apply-templates select="orderingConstraintProof"/>                
        <xsl:choose>
            <xsl:when test="count(trs[1]/rules/*) &gt; 0">
                the
                rule<xsl:if test="count(trs[1]/rules/*) &gt; 1">s</xsl:if> 
                <xsl:apply-templates select="trs[1]/rules/*/.."/>
                remain<xsl:if test="count(trs[1]/rules/*) = 1">s</xsl:if> in R.
            </xsl:when>
            <xsl:otherwise>
                all rules of R could be removed.
            </xsl:otherwise>
        </xsl:choose>
        Moreover,
        <xsl:choose>
            <xsl:when test="count(trs[2]/rules/*) &gt; 0">
                the
                rule<xsl:if test="count(trs[2]/rules/*) &gt; 1">s</xsl:if> 
                <xsl:apply-templates select="trs[2]/rules/*/.."/>
                remain<xsl:if test="count(trs[2]/rules/*) = 1">s</xsl:if> in S.
            </xsl:when>
            <xsl:otherwise>
                all rules of S could be removed.
            </xsl:otherwise>
        </xsl:choose>        
        <p>
            <xsl:apply-templates select="relativeTerminationProof">
                <xsl:with-param name="indent" select="concat($indent, '.1')"/>
            </xsl:apply-templates>
        </p>
    </xsl:template>
    
    <xsl:template match="trsNonterminationProof" mode="relNonterm">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> Disproving termination of R</h3>
        It follows a proof that R is nonterminating. Hence, R/S is not relative terminating.
        <xsl:apply-templates select=".">
            <xsl:with-param name="indent" select="concat($indent, '.1')"/>
        </xsl:apply-templates>
    </xsl:template>
    
    <xsl:template match="ruleRemoval" mode="relNonterm">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> Rule Removal</h3>
        Some rules have been removed and it remains to disprove relative termination of the following TRSs R 
        <xsl:apply-templates select="trs[1]/rules/*/.."/>
        and S 
        <xsl:apply-templates select="trs[2]/rules/*/.."/>
        <p>
            <xsl:apply-templates select="relativeNonterminationProof">
                <xsl:with-param name="indent" select="concat($indent, '.1')"/>
            </xsl:apply-templates>
        </p>
    </xsl:template>
    
    
    <xsl:template match="rIsEmpty">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> R is empty </h3>
        <p>There are no rules in the TRS. Hence, it is terminating.</p>
    </xsl:template>

    <xsl:template match="rIsEmpty" mode="relative">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> R is empty </h3>
        <p>There are no rules in the TRS R. Hence, R/S is relative terminating.</p>
    </xsl:template>

    <xsl:template match="rIsEmpty" mode="complexity">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> R is empty </h3>
        <p>There are no rules in the TRS R. Hence, R/S has complexity O(1).</p>
    </xsl:template>

    <xsl:template match="equalityRemoval" mode="relative">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> Equality Removal </h3>
        <p>All equality rules (of the form t<xsl:value-of select="$rewrite"/>t) are removed from the non-strict rules. </p>
        <p>
            <xsl:apply-templates select="relativeTerminationProof">
                <xsl:with-param name="indent" select="concat($indent, '.1')"/>
            </xsl:apply-templates>
        </p>
    </xsl:template>
    
    <xsl:template match="sIsEmpty" mode="relative">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> S is empty </h3>
        <p>There are no rules in the TRS S. Hence, R/S is relative terminating iff R is terminating.</p>
        <p>
            <xsl:apply-templates select="trsTerminationProof">
                <xsl:with-param name="indent" select="concat($indent, '.1')"/>
            </xsl:apply-templates>
        </p>
    </xsl:template>
    
    
    <xsl:template match="sizeChangeProc">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> Size-Change Termination</h3>
        <p>
        Using size-change termination in combination with
        <xsl:choose>
            <xsl:when test="subtermCriterion">
                the subterm criterion
            </xsl:when>
            <xsl:when test="reductionPair">
                the  
                <xsl:apply-templates select="reductionPair/orderingConstraintProof"/>
                with the following set of usable rules
                <xsl:apply-templates select="reductionPair/usableRules/*"/>
            </xsl:when>
            <xsl:otherwise>
                some unknown technique
            </xsl:otherwise>
        </xsl:choose>
        one obtains the following initial size-change graphs.</p>  
        <ul>
            <xsl:for-each select="sizeChangeGraph">
                <li>
                    <p><xsl:apply-templates select="rule"/>:</p>
                    <xsl:for-each select="edge">
                        <xsl:value-of select="*[1]/text()"/>
                        <xsl:text>&gt;</xsl:text><xsl:if test="strict/text() = 'false'">=</xsl:if>
                        <xsl:value-of select="*[3]/text()"/>                        
                        <xsl:if test="position() != last()">, </xsl:if>
                    </xsl:for-each>
                </li>
            </xsl:for-each>
        </ul>
        
        <p>As there is no critical graph in the transitive closure, there are no infinite chains.
        </p>
    </xsl:template>
    
    <xsl:template match="depGraphProc">
        <xsl:param name="indent"/>
        <h3><xsl:value-of select="$indent"/> Dependency Graph Processor</h3>
        <xsl:variable name="all" select="count(component)"/>
        <xsl:variable name="real" select="count(component[realScc/text() = 'true'])"/>
        <p>The dependency pairs are split into <xsl:value-of select="$real"/>
        component<xsl:if test="$real != 1">s</xsl:if>.</p>
        <xsl:choose>
            <xsl:when test="$real &gt; 0">
                <ul>    
                    <xsl:apply-templates select="." mode="iterate">
                        <xsl:with-param name="count" select="1"/>
                        <xsl:with-param name="indent" select="$indent"/>
                        <xsl:with-param name="index" select="1"/>
                        <xsl:with-param name="n" select="$all"/>
                    </xsl:apply-templates>
                </ul>        
            </xsl:when>
        </xsl:choose>        
    </xsl:template>
    
    <xsl:template mode="iterate" match="depGraphProc">
        <xsl:param name="indent"/>
        <xsl:param name="count"/>
        <xsl:param name="index"/>
        <xsl:param name="n"/>
        <xsl:variable name="newindex" select="$index + count(component[$count]/realScc[text() = 'true'])"/>
        <xsl:if test="$index != $newindex">
            <li>
                The
                <xsl:choose>
                    <xsl:when test="$index = 1">1<sup>st</sup></xsl:when>
                    <xsl:when test="$index = 2">2<sup>nd</sup></xsl:when>
                    <xsl:when test="$index = 3">3<sup>rd</sup></xsl:when>
                    <xsl:otherwise><xsl:value-of select="$index"/><sup>th</sup></xsl:otherwise>
                </xsl:choose>
                component contains the
                pair<xsl:if test="count(component[$count]/dps/rules/rule) &gt; 1">s</xsl:if>
                <xsl:apply-templates select="component[$count]/dps/*"/>
                <xsl:variable name="sub_index" select="count(component[$count]/*)"/>
                <xsl:apply-templates select="component[$count]/*[$sub_index]">
                    <xsl:with-param name="indent" select="concat($indent, '.', $index)"/>   
                </xsl:apply-templates>
            </li>
        </xsl:if>
        <xsl:if test="$count &lt; $n">
            <xsl:apply-templates select="." mode="iterate">
                <xsl:with-param name="indent" select="$indent"/>
                <xsl:with-param name="count" select="$count + 1"/>
                <xsl:with-param name="index" select="$newindex"/>
                <xsl:with-param name="n" select="$n"/>
            </xsl:apply-templates>
        </xsl:if>        
    </xsl:template>
    
    <!-- variables always in red -->
    <xsl:template name="var" match="var">
        <span class="var"><xsl:value-of select="."/></span>
    </xsl:template>
    

    <xsl:template match="labeling">
        <span class="label">
            <xsl:if test="count(number) != 1"><xsl:text>(</xsl:text></xsl:if>
            <xsl:for-each select="number">
                <xsl:apply-templates/>
                <xsl:if test="position() != last()">,</xsl:if>                    
            </xsl:for-each>
            <xsl:if test="count(number) != 1"><xsl:text>)</xsl:text></xsl:if>
            <xsl:if test="position() != last()">,</xsl:if>
        </span>
    </xsl:template>

    <xsl:template match="sharp">
        <span class="dp_fun">
            <xsl:apply-templates select="*[1]">
                <xsl:with-param name="sharp">true</xsl:with-param>
            </xsl:apply-templates>
            <sup>#</sup>
        </span>        
    </xsl:template>

    <xsl:template match="name">
        <xsl:param name="sharp">false</xsl:param>
        <xsl:choose>
            <xsl:when test="$sharp = 'true'">
                <xsl:value-of select="text()"/>                
            </xsl:when>
            <xsl:otherwise>
                <span class="fun">
                    <xsl:value-of select="text()"/>
                </span>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="numberLabel">
        <span class="label">
            <xsl:apply-templates/>
        </span>
    </xsl:template>
    <xsl:template match="symbolLabel">
        <span class="label">
            <xsl:apply-templates/>
        </span>
    </xsl:template>
    
    <xsl:template match="labeledSymbol">
        <xsl:param name="sharp">false</xsl:param>
        <xsl:apply-templates select="*[1]">
            <xsl:with-param name="sharp" select="$sharp"/>
        </xsl:apply-templates>
        <sub>
            <xsl:apply-templates select="*[2]"/>
        </sub>
    </xsl:template>
    
    <xsl:template match="funapp">
      <xsl:apply-templates select="*[1]"/>
      <xsl:if test="count(arg) &gt; 0">
      <xsl:text>(</xsl:text>
      <xsl:for-each select="arg">
        <xsl:apply-templates/>
        <xsl:if test="position() != last()">,</xsl:if>
      </xsl:for-each>
      <xsl:text>)</xsl:text>
      </xsl:if>
    </xsl:template>
    
    <xsl:template match="box">
        <span style="color:purple"><xsl:value-of select="$box"/></span>
    </xsl:template>
    
    <xsl:template match="funContext">
        <xsl:apply-templates select="*[1]"/>
        <xsl:text>(</xsl:text>
        <xsl:for-each select="before/*">
            <xsl:apply-templates select="."/><xsl:text>,</xsl:text>
        </xsl:for-each>
        <xsl:apply-templates select="*[3]"/>
        <xsl:for-each select="after/*">
            <xsl:text>,</xsl:text><xsl:apply-templates select="."/>
        </xsl:for-each>
        <xsl:text>)</xsl:text>
    </xsl:template>
    
    <xsl:template match="rule">
        <xsl:apply-templates select="lhs"/>
        <xsl:value-of select="$rewrite"/>
        <xsl:apply-templates select="rhs"/>
    </xsl:template>
    
    <xsl:template match="rule" mode="centered">
        <table align="center">
            <tr>
                <td><xsl:apply-templates select="lhs"/></td>
                <td><xsl:value-of select="$rewrite"/></td>
                <td><xsl:apply-templates select="rhs"/></td>
            </tr>
        </table>
    </xsl:template>
    
    <xsl:template match="equations">
        <xsl:for-each select="rules">
            <xsl:call-template name="rules">
                <xsl:with-param name="arr">=</xsl:with-param>
                <xsl:with-param name="name">equations</xsl:with-param>
            </xsl:call-template>
        </xsl:for-each>
    </xsl:template>    
        
    <xsl:template match="rules">
        <xsl:call-template name="rules">
            <xsl:with-param name="arr"><xsl:value-of select="$arrow"/></xsl:with-param>
            <xsl:with-param name="name">rules</xsl:with-param>
        </xsl:call-template>
    </xsl:template>

    <xsl:template match="conditionalRule">
        <xsl:for-each select="rule">
            <xsl:if test="position() = 2">
                <xsl:text> | </xsl:text>
            </xsl:if>
            <xsl:if test="position() &gt; 2">
                <xsl:text>, </xsl:text>
            </xsl:if>
            <xsl:apply-templates select="lhs"/>
            <xsl:value-of select="$arrow"/>
            <xsl:if test="position() != 1"><sup>*</sup></xsl:if>
            <xsl:apply-templates select="rhs"/>
        </xsl:for-each>
    </xsl:template>
    
    <xsl:template match="conditionalRules">
        <table>
            <xsl:attribute name="align">center</xsl:attribute>
            <xsl:for-each select="conditionalRule">
                <tr>
                    <xsl:for-each select="rule">
                        <xsl:if test="position() = 2">
                            <td>|</td>
                        </xsl:if>
                        <xsl:if test="position() &gt; 2">
                            <td>,</td>
                        </xsl:if>
                        <td align="right">
                            <xsl:apply-templates select="lhs"/>
                        </td>
                        <td align="center">
                            <xsl:value-of select="$arrow"/>
                            <xsl:if test="position() != 1"><sup>*</sup></xsl:if>
                        </td>
                        <td align="left">
                            <xsl:apply-templates select="rhs"/>
                        </td>
                    </xsl:for-each>
                </tr>
            </xsl:for-each>
        </table>
    </xsl:template>
    
    <xsl:template name="rules">
        <xsl:param name="arr"/>
        <xsl:param name="name"/>        
            <xsl:choose>
                <xsl:when test="count(rule) = 0">
                    <p>
                    <xsl:text>There are no </xsl:text>
                    <xsl:value-of select="$name"/>
                    <xsl:text>.</xsl:text>
                    </p>
                </xsl:when>
                <xsl:otherwise>
                    <table>
                        <xsl:attribute name="align">center</xsl:attribute>
                        <xsl:for-each select="rule">
                            <tr>
                                <td align="right">
                                    <xsl:apply-templates select="lhs"/>
                                </td>
                                <td align="center">
                                    <xsl:value-of select="$arr"/>
                                </td>
                                <td align="left">
                                    <xsl:apply-templates select="rhs"/>
                                </td>
                            </tr>
                        </xsl:for-each>
                    </table>
                </xsl:otherwise>
            </xsl:choose>        
    </xsl:template>
    
    <xsl:template match="substitution">
        <xsl:choose>
            <xsl:when test="count(substEntry) = 0">
                <xsl:value-of select="$emptyset"/>
            </xsl:when>
            <xsl:otherwise>    
                {<xsl:for-each select="substEntry">
                    <xsl:apply-templates select="*[1]"/>/<xsl:apply-templates select="*[2]"/>
                    <xsl:if test="last() != position()">, </xsl:if>
                </xsl:for-each>}
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>
    
    
</xsl:stylesheet>
