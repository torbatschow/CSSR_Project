# CSSR Assignment
## Overview

This is the repository for the final project and the assignment leading to it in the [MPP-E1180: Introduction to Collaborative Social Science Data Analysis](https://github.com/HertieDataScience) course of the fall semester 2016. It is authored by [Alex](https://github.com/corrod3) and [Torben](https://github.com/torbatschow). Any feedback, comments, and ideas are highly appreciated. Please send them to <a href="mailto:t.klausa@mpp.hertie-school.org">Torben</a> or <a href="mailto:a.sacharow@mpp.hertie-school.org">Alex</a> or submit a pull request.

## Idea

Our aim is to research the impact of alcohol regulation on health and crime in Germany. We will in particular look at the effects of the night sale prohibition in the state of Baden-Wuerttenberg which was implemented in 2010 and the prohibition to drink alcohol in the Public in Berlin which lasted from 1999 to 2006. For the first effect has already been researched by [Marcus and Siedler (2014)](http://www.sciencedirect.com/science/article/pii/S0047272714002564). Our analysis will reproduce their approach, however in less depth. The second effect ... [check].

## Data

For the implementation of the project we need three different sorts of data: First, the data on alcohol regulation. To our knowledge there is no processed database of alcohol regulation in Germany. Hence, we gather the data from different publically available sources, e.g. news. Second, to estimate the effects on health we use [hospital diagnosis statistics](https://www.destatis.de/DE/Publikationen/Thematisch/Gesundheit/Krankenhaeuser/DiagnosedatenKrankenhaus.html) from the Statistische Bundesamt Destatis. The diagnosis statistics are archived by the [Statistische Bibliothek](https://www.destatis.de/GPStatistik/receive/DESerie_serie_00000950?list=all). Like Marcus and Siedler (2014) and [Wicki and Gmel (2011)](refhub.elsevier.com/S0047-2727(14)00256-4/rf0235) we use the  codes F10 ("Mental and behavioral disorders due to alcohol use") and T51 ("Toxic effect of alcohol") of the 3-digital ICD-10 classification ("International Statistical Classification of Diseases and Related Health Problems") to define alcohol related hospitalizations. Third, for the effects on crime we use the [Polizeiliche Kriminalstatistik (PKS)](https://www.bka.de/DE/AktuelleInformationen/StatistikenLagebilder/PolizeilicheKriminalstatistik/pks_node.html;jsessionid=5DFB83E1B040E5C0CCF7F9DF99203DA1.live0612) by the BKA.

## Assignment Tasks

### Second Assignment
The second pair assignment is a proposal for your Collaborative Research Project. It is an opportunity for you to layout your collaborative research paper question, justify why it is interesting, provide basic literature review (properly cited using BibTeX), and identify data sources/methodologies that you can access to help answer your question. You will also demonstrate your understanding of literate programming technologies. Deadline 28 October, 2,000 words maximum, 10% of final grade.

### Third Assignment

In the third pair assignment you will gather web based data from at least two sources, merge the data sets, conduct basic inferential statistics on the data to address a relevant research question and briefly describe the results including with dynamically generated tables and figures. Students are encouraged to access data and perform statistical analyses with an eye to answering questions relevant for their Collaborative Research Project. Deadline: 11 November. The write up should be 1,500 words maximum and use literate programming, 10% of final grade.

### Final collaborative Research Project

For the Collaborative Research Project you will pose an interesting social science question and attempt to answer it using standard academic practices including original data collection and statistical analysis. The project should be considered a 'dry run' for your thesis. The project has three presentation outputs designed to present your research to multiple audiences. The first is an oral presentation (10 minute maximum) given in the final class. The second is a standard academic paper (5,000 words maximum) that is fully reproducible and dynamically generated. The third is a website designed to present key aspects of your research in an engaging way to a general audience. The paper and website are due in the Final Exam Week. The presentation and website are each worth 10% of your final mark. The paper is worth 30%.

## Disclaimer

The idea come from a list of possible thesis topics. Even though neighter of us intends to write his thesis on the topic, we found it interesting and wanted to pursue it in the framework of the CSSR project. The following text was our starting point:

Regulation of Alcohol Access and its Impact on Crime and Health

Different German states and cities introduced different types of local and temporal sales and consumption regulations (e.g., no alcohol on public transports) at different points in time. The aim of this project is to study the impact of such regulations on alcohol related crimes.
Note: There is no need to focus on Germany! Similar policy experiments might have also happened in other European or non-European countries.
One would have to (a) build up a data-set that captures the details of these drinking bans/ alcohol regulations and (b) compile detailed, city level crime data (e.g., in Germany from the PKS) and, e.g., data on hospital admissions, etc.
