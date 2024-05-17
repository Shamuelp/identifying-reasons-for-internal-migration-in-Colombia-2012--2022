# identifying-reasons-for-internal-migration-in-Colombia-2012--2022
Through basic data management, tables are created that allow observing the behavior of internal migration in Colombia, segregated by age in an interval of "&lt;20", "20 - 30", "30 - 40", "40 - 50", "50 - 60", "60 - 70", ">70" years, using data from DANE (National Administrative Department of Statistics). The exercise has been carried out in the R programming language.

The data can be found on the DANE portal at the following link: https://microdatos.dane.gov.co/index.php/catalog/central/about

Although the quality of life survey used for the exercise has changed over the years, I have built a classification form that allows the variable that contains the reasons to be reorganized so that we can contrast the data over the years without major problems.

In this variable (P5739 for 2012, P6096 for 2014, 2016, 2018, 2020 and P1662 for 2022) there are data of 1:11 that represent the reasons why people have been displaced in the national territory, as seen below:

1 = Difficulty finding work or livelihood
2 = Risk or consequence of natural disaster
3 = Violent threat against your integrity
4 = Education
5 = He started a family
6 = Health
7 = Improve housing or location
8 = Best job opportunities
9 = Accompany another household member
10 = Acquisition of housing
11 = Other

Finally we end up with tables segregated by age ranges. Keep in mind that the years in which any of the 11 reasons are not found is because at that time the question was not applied in the survey.

Created by: Shamuel Molina Duque.
Economics and data science student.
