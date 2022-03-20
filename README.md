Predicting MLB Home Runs - Kyle Hirsch and Ian Suleski

Baseball has always been an analytically driven sport with public statistics for virtually every action a player can take, and players being judged mainly on averages and offensive output. With a total of 162 games per regular season for each team, player consistency is key, and player value is determined mainly on consistency. Consistency at high levels in certain statistics hold more weight, such as statistics that measure offensive output. Players with higher measures of offensive output generate more runs for their team, which means more wins. As a tool for identifying player value, this study will attempt to construct a model that is able to predict a players' home run output for an upcoming season. The task of the model to be created in this project will be to predict home runs per plate appearance in an upcoming season for any MLB player given their offensive stats from the four sequential seasons immediately preceding. It is essential to predict home runs as a percentage of total plate appearances because total plate appearances can vary drastically from season to season. 

The data used in this study represents offensive production by players in the MLB between the 2010 and 2013 seasons. More specifically, the statistics included in this study are home runs (HR), runs batted in (RBI), walk percentage (BB%), strike-out percentage (K%), batting average (AVG), on-base percentage (OBP), slugging percentage (SLG), and plate appearances (PA). Home runs (HR) are the dependent variable this study is trying to predict.This data has been structured and presented on FanGraphs, a website that specializes in providing baseball data and statistics, which is where the data for this study has been acquired (FanGraphs, 2021). For the purpose of simplifying the data set, this study will only focus on a four season interval: 2010-2013. This interval should provide sufficient data to work with and justify the findings. In order to avoid blatant outliers and inaccurate data, players included in this study must have had at least 200 PA in each season they participated in. This study aims to use four seasons worth of hitting data from a specific player (2010-2013) to predict that player's total home runs in the future season. 

Rstudio is used to build the predictive model and run all of the code in this project. It is recommended to have the latest version of Rstudio software to run the code for this project. The datasets included in this repository are what each of the two models in this study are run on. The first dataset "Combined Baseballdata1.csv" is imported for the initial model build and validation steps. The second dataset "FinalModel_data.csv" is imported for the subsequent model build and validation steps.

Make sure the datasets are saved to your computer and the correct file paths are written during the importing section of the code in Rstudio. This is crucial to the code running correctly. Once the datasets are successfully imported, it is also crucial to install the R libraries with the code provided. These libraries automatically import certain functions that allow the code to run properly. Then, follow the detailed comments in the code "Predicting MLB Home Runs.R" provided in the repository. These comments organize and detail the building and validation steps for both models used in this project.

In conclusion, the model proposed in this project has been proven by multiple sources of validation to be accurate and reliable. The model was tested against two different sets of data consisting of the same number of years in a different time frame. Both sets of data were tested against various validation methods, including tests against multicollinearity, partial least squares, autocorrelation, and heteroscedasticity. Each of these validation methods produced nearly identical results for both sets of data. The model has been proven to be accurate and reliable in predicting future home run outputs for an MLB player, given that player has participated in each of the previous four seasons with at least 200 plate appearances in each season.

References
FanGraphs. (2021, September 9). Major League leaderboards " 2010 batters " Dashboard: FanGraphs baseball. Retrieved September 10, 2021, from https://www.fangraphs.com/leaders.aspx?pos=all&amp;stats=bat&amp;lg=all&amp;qual=y&amp;type=8&amp;season=2010&amp;month=0&amp;season1=2010&amp;ind=0&amp;team=0&amp;rost=0&amp;age=0&amp;filter=&amp;players=0&amp;startdate=&amp;enddate=.  
