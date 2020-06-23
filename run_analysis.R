merge_set = function()
{
    X_train = read.table('/UCI HAR Dataset/train/X_train.txt',sep='\t')
    y_train = read.table('/UCI HAR Dataset/train/y_train.txt',sep='\t')
    X_test = read.table('/UCI HAR Dataset/train/X_test.txt',sep='\t')
    y_test = read.table('/UCI HAR Dataset/train/y_test.txt',sep='\t')
    X = rbind(X_train,X_test)
    y = rbind(y_train,y_test)
    return (X)
}

