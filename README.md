# CSSR Assignment
## Overview

This is the repository for the final project and the assignment leading to it in the [MPP-E1180: Introduction to Collaborative Social Science Data Analysis](https://github.com/HertieDataScience) course of the fall semester 2016. It is authored by [Alex](https://github.com/corrod3) and [Torben](https://github.com/torbatschow). Any feedback, comments, and ideas are highly appreciated. Please send them to <a href="mailto:t.klausa@mpp.hertie-school.org">Torben</a> or <a href="mailto:a.sacharow@mpp.hertie-school.org">Alex</a> or submit a pull request.

The second assignment can be found [here](Pair_Assignment_2.rmd).

## Idea

Our aim is to research the correlation between alcohol consumption and socio-economic factors as well as regulatory measures in Germany. We will in particular look at the effects of the night sale prohibition in the state of Baden-Wuerttenberg which was implemented in 2010. 

The first effect of night sale ban on alcohol-related hospitalization has already been researched by [Marcus and Siedler (2014)](http://www.sciencedirect.com/science/article/pii/S0047272714002564). The freely available working paper can be found [here](https://www.diw.de/documents/publikationen/73/diw_01.c.494858.de/dp1443.pdf). Our analysis will reproduce their approach.

## Data

For the implementation of the project we need three different sorts of data: 

First, the data on alcohol regulation in Baden-Wuerttemberg, which is gathered from publically available news sources. 

Second, to estimate the effects on health we use [hospital diagnosis statistics](https://www.destatis.de/DE/Publikationen/Thematisch/Gesundheit/Krankenhaeuser/DiagnosedatenKrankenhaus.html) from the Statistische Bundesamt Destatis. The diagnosis statistics are archived by the [Statistische Bibliothek](https://www.destatis.de/GPStatistik/receive/DESerie_serie_00000950?list=all). Building upon Marcus and Siedler (2014) and [Wicki and Gmel (2011)](refhub.elsevier.com/S0047-2727(14)00256-4/rf0235) we use the codes *F10.0* (Acute intoxication) and *F10.2* (Dependence syndrome), and *K70* (Alcoholic liver disease) of the 3-digital ICD-10 classification ("International Statistical Classification of Diseases and Related Health Problems") to define alcohol related hospitalizations. 

Third, the data on socio-economic factors includes the respective state's population, population density, unemployment rates of different age groups, the state GDP and beer sales by state. We are aware of the fact that other alcoholic beverages besides beer probably play a role in the cases under scrutiny. However, with these numbers not available, the beer consumption serves as a proxy for general alcohol consumption in the state. We are collecting the supplementary data from different statistics provided by *Destatis*.


## Assignment Tasks

### [Second Assignment](Pair_Assignment_2.rmd)
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
