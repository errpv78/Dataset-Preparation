# DataSet - info
# http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

# Important Points
# 1) Total No of Instances (train+test: 10299
# 2) Total No of Attributes: 561
# Total NA values: N/A





## LOADING THE DATA
# Loading the column names
features = read.table("UCI HAR Dataset/features.txt", col.names = c('index', 'features'))
# > dim(features)
# [1] 561   2
# 
# Inference
"Total 561 attributes found with column names having slight
descriptive issues. To do make column names more descriptive for
tidy dataset."

# Loading the different type of activities
activities = read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("activity_code", "activity"))
#
# Inference
# Total 6 types of activities
# > activities
# activity_code           activity
# 1             1            WALKING
# 2             2   WALKING_UPSTAIRS
# 3             3 WALKING_DOWNSTAIRS
# 4             4            SITTING
# 5             5           STANDING
# 6             6             LAYING


# Load Training Data 
# Loading Training Subjects
subject_train = read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
"> dim(subject_train) 
[1] 7352    1
> table(subject_train)
subject_train
  1   3   5   6   7   8  11  14  15  16  17  19  21  22  23  25  26 
347 341 302 325 308 281 316 323 328 366 368 360 408 321 372 409 392 
 27  28  29  30 
376 382 344 383 
"
# Inference
# Total 7352 instances in training data


# Loading Training X with column names as given by features table
X_train = read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$features)
"> dim(X_train)
[1] 7352  561"
# Inference 
# Table dim matches with given total features (561) and total
# training instances 7352


# Loading y_train
y_train = read.table("UCI HAR Dataset/train/y_train.txt", col.names = "activity_code")
"> dim(y_train)
[1] 7352    1
> table(y_train)
y_train
   1    2    3    4    5    6 
1226 1073  986 1286 1374 1407 "
# Inference
# All the activity in y_train are as per activity code described 
# by activities table (ie in range 1-6)


# Loading Test Data
subject_test = read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
"> dim(subject_test)
[1] 2947    1
> 2947+7352
[1] 10299"
# Inference
# Total test instances = 2947
# Total instances matches with given description


# Loading X_test
X_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$features)
"> dim(X_test)
[1] 2947  561"


# Loading y_test
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "activity_code")
"> dim(y_test)
[1] 2947    1
> table(y_test)
y_test
  1   2   3   4   5   6 
496 471 420 491 532 537 "


## Replacing activity code in y_train, y_test by activity name
# Selecting activities
activity = activities[,2]
"> activity
[1] 'WALKING'            'WALKING_UPSTAIRS'   'WALKING_DOWNSTAIRS'
[4] 'SITTING'            'STANDING'           'LAYING'   "

# Replacing activity code with activity name in y_train
y_train[,] = activity[y_train[,]]
"> head(y_train,n=4)
  activity_code
1      STANDING
2      STANDING
3      STANDING
4      STANDING

> table(y_train)
y_train
            LAYING            SITTING           STANDING 
              1407               1286               1374 
           WALKING WALKING_DOWNSTAIRS   WALKING_UPSTAIRS 
              1226                986               1073 
"

# Replacing activity code with activity name in y_test
y_test[,] = activity[y_test[,]]
"> head(y_test,n=5)
activity_code
1      STANDING
2      STANDING
3      STANDING
4      STANDING
5      STANDING
> table(y_test)
y_test
LAYING            SITTING           STANDING 
537                491                532 
WALKING WALKING_DOWNSTAIRS   WALKING_UPSTAIRS 
496                420                471 "



## MERGING THE DATA
# Merging X_train, y_train, subject_train
train = cbind(y_train, X_train, subject_train)
"> dim(train)
[1] 7352  563"
# Merging X_test, y_test, subject_test
test = cbind(y_test, X_test, subject_test)
"> dim(test)
[1] 2947  563"
# Merging train and test
TidyData = rbind(train, test)
"> dim(TidyData)
[1] 10299   563"



## FILTERING THE REQUIRED COLUMNS FOR FINAL TIDY DATA
# Selecting activity column
act = TidyData[,1]
# Selecting subject column
sub = TidyData[,'subject']
# Selecting Mean columns
M = TidyData[,grepl('Mean',names(TidyData))]
# Selecting mean columns
m = TidyData[,grepl('mean',names(TidyData))]
# Selecting standard deviation columns
s = TidyData[,grepl('std',names(TidyData))]
# Creating Filtered TidyData
TidyData = cbind(act, sub, M, m, s)
"> dim(TidyData)
[1] 10299    88"
# Inference
# Final Tidy data has 10299 rows and 8 columns


## LABELING DATA WITH DESCRIPTIVE NAMES
names(TidyData)[1] = "activity"
names(TidyData)[2] = "subject"
names(TidyData)<-gsub("Acc", "Accelerometer", names(TidyData))
names(TidyData)<-gsub("Gyro", "Gyroscope", names(TidyData))
names(TidyData)<-gsub("BodyBody", "Body", names(TidyData))
names(TidyData)<-gsub("Mag", "Magnitude", names(TidyData))
names(TidyData)<-gsub("^t", "Triaxial", names(TidyData))
names(TidyData)<-gsub("^f", "Frequency", names(TidyData))
names(TidyData)<-gsub("tBody", "TimeBody", names(TidyData))
names(TidyData)<-gsub("-mean()", "Mean", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("-std()", "STD", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("-freq()", "Frequency", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("angle", "Angle", names(TidyData))
names(TidyData)<-gsub("gravity", "Gravity", names(TidyData))



## SUMMARIZING THE FINAL DATA
FINAL_DATA = TidyData %>% group_by(.dots=c('activity', 'subject')) %>% summarise_all(funs(mean))
FINAL_DATA = data.frame(FINAL_DATA)
"> dim(FINAL_DATA)
[1] 180  88


> head(FINAL_DATA)
  activity subject Angle.TimeBodyAccelerometerMean.Gravity. Angle.TimeBodyAccelerometerJerkMean..GravityMean.
1   LAYING       1                              0.021365966                                       0.003060407
2   LAYING       2                              0.005791810                                      -0.006355576
3   LAYING       3                              0.017895938                                       0.016647953
4   LAYING       4                             -0.002357753                                      -0.015503757
5   LAYING       5                              0.021209193                                       0.055364796
6   LAYING       6                              0.004989439                                       0.032217035
  Angle.TimeBodyGyroscopeMean.GravityMean. Angle.TimeBodyGyroscopeJerkMean.GravityMean. Angle.X.GravityMean. Angle.Y.GravityMean.
1                             -0.001666985                                   0.08443716            0.4267062           -0.5203438
2                              0.065286011                                  -0.02937101            0.6174234           -0.5197326
3                              0.045587477                                   0.03172617            0.4235059           -0.6300628
4                              0.007129605                                   0.03665673            0.5534898           -0.7632172
5                             -0.005812889                                  -0.03444203            0.5987327           -0.8252999
6                              0.085938987                                  -0.03505668            0.5933944           -0.8745677
  Angle.Z.GravityMean. TriaxialBodyAccelerometer.mean...X TriaxialBodyAccelerometer.mean...Y TriaxialBodyAccelerometer.mean...Z
1           -0.3524131                          0.2215982                        -0.04051395                         -0.1132036
2           -0.4789282                          0.2813734                        -0.01815874                         -0.1072456
3           -0.3462431                          0.2755169                        -0.01895568                         -0.1013005
4           -0.2297904                          0.2635592                        -0.01500318                         -0.1106882
5           -0.1681463                          0.2783343                        -0.01830421                         -0.1079376
6           -0.1066244                          0.2486565                        -0.01025292                         -0.1331196
  TriaxialGravityAccelerometer.mean...X TriaxialGravityAccelerometer.mean...Y TriaxialGravityAccelerometer.mean...Z
1                            -0.2488818                             0.7055498                             0.4458177
2                            -0.5097542                             0.7525366                             0.6468349
3                            -0.2417585                             0.8370321                             0.4887032
4                            -0.4206647                             0.9151651                             0.3415313
5                            -0.4834706                             0.9548903                             0.2636447
6                            -0.4767099                             0.9565938                             0.1758677
  TriaxialBodyAccelerometerJerk.mean...X TriaxialBodyAccelerometerJerk.mean...Y TriaxialBodyAccelerometerJerk.mean...Z
1                             0.08108653                            0.003838204                            0.010834236
2                             0.08259725                            0.012254788                           -0.001802649
3                             0.07698111                            0.013804101                           -0.004356259
4                             0.09344942                            0.006933132                           -0.006410543
5                             0.08481648                            0.007474608                           -0.003040672
6                             0.09634820                           -0.001145292                            0.003288173
  TriaxialBodyGyroscope.mean...X TriaxialBodyGyroscope.mean...Y TriaxialBodyGyroscope.mean...Z TriaxialBodyGyroscopeJerk.mean...X
1                   -0.016553094                    -0.06448612                      0.1486894                         -0.1072709
2                   -0.018476607                    -0.11180082                      0.1448828                         -0.1019741
3                   -0.020817054                    -0.07185072                      0.1379996                         -0.1000445
4                   -0.009231563                    -0.09301282                      0.1697204                         -0.1050199
5                   -0.021893501                    -0.07987096                      0.1598944                         -0.1021141
6                   -0.007960503                    -0.10721832                      0.1791021                         -0.1112673
  TriaxialBodyGyroscopeJerk.mean...Y TriaxialBodyGyroscopeJerk.mean...Z TriaxialBodyAccelerometerMagnitude.mean..
1                        -0.04151729                        -0.07405012                                -0.8419292
2                        -0.03585902                        -0.07017830                                -0.9774355
3                        -0.03897718                        -0.06873387                                -0.9727913
4                        -0.03812304                        -0.07121563                                -0.9545576
5                        -0.04044469                        -0.07083097                                -0.9667779
6                        -0.04241043                        -0.07177747                                -0.9188789
  TriaxialGravityAccelerometerMagnitude.mean.. TriaxialBodyAccelerometerJerkMagnitude.mean.. TriaxialBodyGyroscopeMagnitude.mean..
1                                   -0.8419292                                    -0.9543963                            -0.8747595
2                                   -0.9774355                                    -0.9877417                            -0.9500116
3                                   -0.9727913                                    -0.9794846                            -0.9515648
4                                   -0.9545576                                    -0.9700958                            -0.9302365
5                                   -0.9667779                                    -0.9801413                            -0.9469383
6                                   -0.9188789                                    -0.9547505                            -0.9089802
  TriaxialBodyGyroscopeJerkMagnitude.mean.. FrequencyBodyAccelerometer.mean...X FrequencyBodyAccelerometer.mean...Y
1                                -0.9634610                          -0.9390991                          -0.8670652
2                                -0.9917671                          -0.9767251                          -0.9798009
3                                -0.9867136                          -0.9806656                          -0.9611700
4                                -0.9850685                          -0.9588021                          -0.9388834
5                                -0.9864194                          -0.9687417                          -0.9654195
6                                -0.9556457                          -0.9391143                          -0.9237068
  FrequencyBodyAccelerometer.mean...Z FrequencyBodyAccelerometer.meanFreq...X FrequencyBodyAccelerometer.meanFreq...Y
1                          -0.8826669                             -0.15879267                              0.09753484
2                          -0.9843810                             -0.14648279                              0.25728947
3                          -0.9683321                             -0.07395264                              0.23847075
4                          -0.9675043                             -0.27419462                              0.36623145
5                          -0.9770077                             -0.13563245                              0.46652823
6                          -0.9380449                             -0.21972993                              0.34841875
  FrequencyBodyAccelerometer.meanFreq...Z FrequencyBodyAccelerometerJerk.mean...X FrequencyBodyAccelerometerJerk.mean...Y
1                              0.08943766                              -0.9570739                              -0.9224626
2                              0.40253255                              -0.9858136                              -0.9827683
3                              0.21697167                              -0.9805132                              -0.9687521
4                              0.20132959                              -0.9785425                              -0.9439700
5                              0.13231087                              -0.9826897                              -0.9653286
6                              0.16145793                              -0.9670724                              -0.9360434
  FrequencyBodyAccelerometerJerk.mean...Z FrequencyBodyAccelerometerJerk.meanFreq...X FrequencyBodyAccelerometerJerk.meanFreq...Y
1                              -0.9480609                                  0.13241909                                  0.02451362
2                              -0.9861971                                  0.15980833                                  0.12120642
3                              -0.9791223                                  0.17597855                                 -0.01317750
4                              -0.9753833                                  0.18243648                                  0.09874288
5                              -0.9832503                                  0.23991516                                  0.19567734
6                              -0.9544258                                  0.01147319                                 -0.02220295
  FrequencyBodyAccelerometerJerk.meanFreq...Z FrequencyBodyGyroscope.mean...X FrequencyBodyGyroscope.mean...Y FrequencyBodyGyroscope.mean...Z
1                                  0.02438795                      -0.8502492                      -0.9521915                      -0.9093027
2                                  0.19055822                      -0.9864311                      -0.9833216                      -0.9626719
3                                  0.04481969                      -0.9701673                      -0.9780997                      -0.9623420
4                                  0.07702112                      -0.9672037                      -0.9721878                      -0.9614793
5                                  0.09169388                      -0.9757975                      -0.9782496                      -0.9632029
6                                  0.07846840                      -0.9354398                      -0.9417715                      -0.9326366
  FrequencyBodyGyroscope.meanFreq...X FrequencyBodyGyroscope.meanFreq...Y FrequencyBodyGyroscope.meanFreq...Z
1                        -0.003546796                         -0.09152913                          0.01045813
2                         0.102611319                          0.04228067                          0.05529860
3                        -0.082216645                         -0.02668201                          0.14768646
4                        -0.066092182                         -0.52689000                          0.15288631
5                        -0.022723586                          0.06812485                          0.04136003
6                         0.102549066                          0.02365678                          0.04452255
  FrequencyBodyAccelerometerMagnitude.mean.. FrequencyBodyAccelerometerMagnitude.meanFreq.. FrequencyBodyAccelerometerJerkMagnitude.mean..
1                                 -0.8617676                                     0.08640856                                     -0.9333004
2                                 -0.9751102                                     0.26629821                                     -0.9853741
3                                 -0.9655243                                     0.23699013                                     -0.9759496
4                                 -0.9393897                                     0.24169790                                     -0.9622871
5                                 -0.9622350                                     0.29203209                                     -0.9773564
6                                 -0.9123517                                     0.14460509                                     -0.9486555
  FrequencyBodyAccelerometerJerkMagnitude.meanFreq.. FrequencyBodyGyroscopeMagnitude.mean.. FrequencyBodyGyroscopeMagnitude.meanFreq..
1                                          0.2663912                             -0.8621902                                -0.13977501
2                                          0.3417586                             -0.9721130                                 0.01856447
3                                          0.2386111                             -0.9645867                                -0.02292961
4                                          0.2740273                             -0.9615567                                -0.25985197
5                                          0.1970050                             -0.9682571                                 0.10244177
6                                          0.1825251                             -0.9301536                                 0.11931752
  FrequencyBodyGyroscopeJerkMagnitude.mean.. FrequencyBodyGyroscopeJerkMagnitude.meanFreq.. TriaxialBodyAccelerometer.std...X
1                                 -0.9423669                                     0.17648591                        -0.9280565
2                                 -0.9902487                                     0.26480151                        -0.9740595
3                                 -0.9842783                                     0.11069770                        -0.9827766
4                                 -0.9836091                                     0.20294938                        -0.9541937
5                                 -0.9846180                                     0.02473671                        -0.9659345
6                                 -0.9536960                                     0.16376532                        -0.9340494
  TriaxialBodyAccelerometer.std...Y TriaxialBodyAccelerometer.std...Z TriaxialGravityAccelerometer.std...X TriaxialGravityAccelerometer.std...Y
1                        -0.8368274                        -0.8260614                           -0.8968300                           -0.9077200
2                        -0.9802774                        -0.9842333                           -0.9590144                           -0.9882119
3                        -0.9620575                        -0.9636910                           -0.9825122                           -0.9812027
4                        -0.9417140                        -0.9626673                           -0.9212000                           -0.9698166
5                        -0.9692956                        -0.9685625                           -0.9456953                           -0.9859641
6                        -0.9246448                        -0.9252161                           -0.8877463                           -0.9591620
  TriaxialGravityAccelerometer.std...Z TriaxialBodyAccelerometerJerk.std...X TriaxialBodyAccelerometerJerk.std...Y
1                           -0.8523663                            -0.9584821                            -0.9241493
2                           -0.9842304                            -0.9858722                            -0.9831725
3                           -0.9648075                            -0.9808793                            -0.9687107
4                           -0.9761766                            -0.9783028                            -0.9422095
5                           -0.9770766                            -0.9833079                            -0.9645604
6                           -0.9281307                            -0.9663411                            -0.9336745
  TriaxialBodyAccelerometerJerk.std...Z TriaxialBodyGyroscope.std...X TriaxialBodyGyroscope.std...Y TriaxialBodyGyroscope.std...Z
1                            -0.9548551                    -0.8735439                    -0.9510904                    -0.9082847
2                            -0.9884420                    -0.9882752                    -0.9822916                    -0.9603066
3                            -0.9820932                    -0.9745458                    -0.9772727                    -0.9635056
4                            -0.9785120                    -0.9731024                    -0.9611093                    -0.9620738
5                            -0.9854194                    -0.9794987                    -0.9774274                    -0.9605838
6                            -0.9596461                    -0.9553782                    -0.9436349                    -0.9391419
  TriaxialBodyGyroscopeJerk.std...X TriaxialBodyGyroscopeJerk.std...Y TriaxialBodyGyroscopeJerk.std...Z TriaxialBodyAccelerometerMagnitude.std..
1                        -0.9186085                        -0.9679072                        -0.9577902                               -0.7951449
2                        -0.9932358                        -0.9895675                        -0.9880358                               -0.9728739
3                        -0.9803286                        -0.9867627                        -0.9833383                               -0.9642182
4                        -0.9751032                        -0.9868556                        -0.9839654                               -0.9312922
5                        -0.9834223                        -0.9837595                        -0.9896796                               -0.9586128
6                        -0.9396116                        -0.9586288                        -0.9595791                               -0.8973262
  TriaxialGravityAccelerometerMagnitude.std.. TriaxialBodyAccelerometerJerkMagnitude.std.. TriaxialBodyGyroscopeMagnitude.std..
1                                  -0.7951449                                   -0.9282456                           -0.8190102
2                                  -0.9728739                                   -0.9855181                           -0.9611641
3                                  -0.9642182                                   -0.9761213                           -0.9542751
4                                  -0.9312922                                   -0.9607864                           -0.9470318
5                                  -0.9586128                                   -0.9774771                           -0.9582879
6                                  -0.8973262                                   -0.9503419                           -0.9209145
  TriaxialBodyGyroscopeJerkMagnitude.std.. FrequencyBodyAccelerometer.std...X FrequencyBodyAccelerometer.std...Y
1                               -0.9358410                         -0.9244374                         -0.8336256
2                               -0.9897181                         -0.9732465                         -0.9810251
3                               -0.9831393                         -0.9836911                         -0.9640946
4                               -0.9826982                         -0.9524649                         -0.9463810
5                               -0.9837714                         -0.9649539                         -0.9729092
6                               -0.9531570                         -0.9324629                         -0.9297112
  FrequencyBodyAccelerometer.std...Z FrequencyBodyAccelerometerJerk.std...X FrequencyBodyAccelerometerJerk.std...Y
1                         -0.8128916                             -0.9641607                             -0.9322179
2                         -0.9847922                             -0.9872503                             -0.9849874
3                         -0.9632791                             -0.9831226                             -0.9710440
4                         -0.9621545                             -0.9800793                             -0.9443669
5                         -0.9658822                             -0.9856253                             -0.9662426
6                         -0.9240047                             -0.9686192                             -0.9357175
  FrequencyBodyAccelerometerJerk.std...Z FrequencyBodyGyroscope.std...X FrequencyBodyGyroscope.std...Y FrequencyBodyGyroscope.std...Z
1                             -0.9605870                     -0.8822965                     -0.9512320                     -0.9165825
2                             -0.9893454                     -0.9888607                     -0.9819106                     -0.9631742
3                             -0.9837119                     -0.9759864                     -0.9770325                     -0.9672569
4                             -0.9802612                     -0.9750947                     -0.9561825                     -0.9658075
5                             -0.9861356                     -0.9807058                     -0.9772578                     -0.9633057
6                             -0.9635675                     -0.9621650                     -0.9453651                     -0.9471368
  FrequencyBodyAccelerometerMagnitude.std.. FrequencyBodyAccelerometerJerkMagnitude.std.. FrequencyBodyGyroscopeMagnitude.std..
1                                -0.7983009                                    -0.9218040                            -0.8243194
2                                -0.9751214                                    -0.9845685                            -0.9610984
3                                -0.9683502                                    -0.9753054                            -0.9554419
4                                -0.9371880                                    -0.9580371                            -0.9471003
5                                -0.9625254                                    -0.9763819                            -0.9592631
6                                -0.9053740                                    -0.9515527                            -0.9286949
  FrequencyBodyGyroscopeJerkMagnitude.std..
1                                -0.9326607
2                                -0.9894927
3                                -0.9825682
4                                -0.9825436
5                                -0.9834345
6                                -0.9555047"


## WRITING FINAL DATA
write.table(FINAL_DATA, file = 'FINAL_DATA.txt', sep="\t", col.names = T)
