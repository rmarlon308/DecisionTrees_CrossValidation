################################################################################
# Title: MLxmp
# Author: John Honaker
# Date: Jun 6, 2014
# Availability: https://github.com/JHonaker/MLxmp/blob/master/DecisionTree/ID3.R
################################################################################

tree <- function(root, branches) {
    structure(list(root=root, branches=branches), class='tree')
}

node <- function(root) {
    structure(list(root=as.character(root)), class='node')
}

information_gain <- function(S) {
    if (!is.factor(S)) S <- as.factor(S)
    
    p <- prop.table(table(S))
    
    -sum(sapply(levels(S),
                function(name) p[name] * log2(p[name]))
    )
}

predict_ID3 <- function(test_obs, id3_tree) {
    traverse <- function(obs, work_tree) {
        count = 0
        while(class(work_tree) != 'node'){
            count = count + 1
            var = work_tree$root
            work_tree = work_tree$branches[[as.character(obs[var])]]
            if(count > 10) work_tree = node("1")
        }
        work_tree$root
    }
    apply(test_obs, 1, traverse, work_tree=id3_tree)
}

most.frq <- function(nbr.class, nbr.distance) {
    uniq <- unique(nbr.class)
    uniq[which.max(tabulate(match(nbr.class, uniq)))]
}

ID3 <- function(dataset, target_attr,
                attributes=setdiff(names(dataset), target_attr)) {

    if (length(attributes) <= 0) {
        return(node(most.frq(dataset[, target_attr])))
    }
    
    if (length(unique(dataset[, target_attr])) == 1) {
        return(node(unique(dataset[, target_attr])[1]))
    }
    
    best_attr <- attributes[which.min(sapply(attributes, information_gain))]
    rem_attrs <- setdiff(attributes, best_attr)
    split_dataset <- split(dataset, dataset[, best_attr])
    branches <- lapply(seq_along(split_dataset), function(i) {
        name <- names(split_dataset)[i]
        branch <- split_dataset[[i]]
        if (nrow(branch) == 0) node(most.frq(dataset[, target_attr]))
        else ID3(branch[, union(target_attr, rem_attrs), drop=FALSE],
                 target_attr,
                 rem_attrs)
    })
    names(branches) <- names(split_dataset)
    
    id3_tree <- tree(root=best_attr, branches=branches)
    id3_tree
}

################################################################################
# Title: MLxmp
# Author: John Honaker
# Date: Jun 6, 2014
# Availability: https://github.com/JHonaker/MLxmp/blob/master/DecisionTree/ID3.R
################################################################################



