## Passing networks using R

![](/plots/France-Belgium.png)

Here you can find a couple of R functions to create customized **passing networks** with both event data by Statsbomb and tracking data by Metrica Sport.

First, i would suggest to read [**this article**](http://datofutbol.cl/passing-networks-r/) in order to get some general context and more details of this work.

Then, you can follow this steps:

1.- Download the files contained in (or pull) this repository, including the codes (\*.R files) and the field background images (\*.JPG files from "fields" folder).

2.- Get the data

**Statsbomb event data**

* Don't forget to read and agree to the [User Agreement](https://github.com/statsbomb/open-data/blob/master/LICENSE.pdf)

* Get the data using the code of the file "**get_SB_event_data_WC2018.R**" (save the file "SB_WC_events_clean.RDS" into a folder named "data")

* If you have any problem with this process you can also review these excellent resources about working with Statsbomb data in R:
[1](https://ryo-n7.github.io/2019-08-21-visualize-soccer-statsbomb-part-1/) & [2](https://github.com/FCrSTATS/StatsBomb_WomensData/blob/master/1.GettingStartedWithStatsBombData.md)

**Metrica Sport data**

* Download the CSV files from [**this repository**](https://github.com/metrica-sports/sample-data)

3.- Create your passing networks graphs

* Open the "**main_code.R**" file, load the necessary packages and call the respective function to create your customized graphs.

* There are a lot of arguments to configurate the creation and outputs of the passing networks, both data selection and aesthetics parts.

* Here a code example using Statsbomb data with the function "**soccerPassNetEventing.R**"

```
soccerPassNetEventing(gameID = 7584, TeamName = "Japan", poss = T, pass_dir = T, convex = T,
                      minPass = 5, node_pos = "origin", nodeFill = "blue", edgeAlpha = 0.5,
                      label = T, shortNames = T,  maxNodeSize = 15, maxEdgeSize = 2.5,  
                      Flipx = F, field = 1)
```                     

![](/plots/Japan-Belgium-ver2.png)

* And here a code using Metrica Sport data with the function "**soccerPassNetTracking.R**"

```
soccerPassNetTracking(gameID = "1", TeamName = "Home", pos_source = "track", node_pos = "origin",
                      context = "attacking", half = "own", field = 2,
                      pass_dir = T, minPass = 2, convex = T, 
                      label = T, shortNames = F, labelSize = 2, 
                      maxNodeSize = 12, maxEdgeSize = 1.5, edgeAlpha = 0.8)
```                     

![](/plots/1-Home-attack-own-ver2.png)

---

This work is in some way a mix of these things:

* A complement to the soccerPassmap function from the [**{soccermatics} package**](https://github.com/JoGall/soccermatics) by Joe Gallagher

* An adaptation to R from the Python code contained in the [**Friends of Tracking repository about passing networks**](https://github.com/Friends-of-Tracking-Data-FoTD/passing-networks-in-python)

* An implementation of different ideas from the football analytics community and myself


Future/pending work:

* Mainly to enable a passes value metric option, like [**VAEP**](https://people.cs.kuleuven.be/~tom.decroos/reports/kdd19_tomd.pdf), [**xT**](https://karun.in/blog/expected-threat.html), [**g+**](https://www.americansocceranalysis.com/home/2020/4/22/37ucr0d5urxxtryn2cfhzormdziphq), [**EPV**](http://www.sloansportsconference.com/wp-content/uploads/2019/02/Decomposing-the-Immeasurable-Sport.pdf) or similar

