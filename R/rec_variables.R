# Recodificación

rec_cat_5a3 <- function(valores,
                        rec = "1:2 = 1;
                               3 = 2;
                               4:5 = 3;
                               else = 9",
                        labels = NULL){

  sjmisc::rec(valores,
              rec = rec) %>%
    haven::labelled(labels = labels,
                    label = get_label(valores))
}
