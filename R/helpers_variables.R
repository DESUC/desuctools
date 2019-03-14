# Funciones de trabajo con datos ------------------------------------------

shift_missing <- function(.data, .x, .y = NULL, missing = c(77L, 88L, 99L)) {
    # Revisa preguntas de respuesta múltiple para corregir respuestas inválidas en variables posteriores.
    missing <- c(missing, NA)
    
    var1 <- enquo(.x)
    var2 <- enquo(.y)
    
    add_labels <- function(x, var_labels = NULL) {
        labelled(x, labels = attr(var_labels, "labels"))
    }
    
    x <- pull(.data, !!var1)
    
    if (rlang::quo_is_null(var2)) {
        
        x_lab <- if_else(x %in% missing, NA_integer_, as.integer(x)) %>% add_labels(x)
        
        .data %>% mutate(`:=`(!!rlang::as_label(var1), x_lab))
        
    } else {
        y <- pull(.data, !!var2)
        
        x_lab <- if_else(x %in% missing & !(y %in% missing), as.integer(y), as.integer(x)) %>% add_labels(x)
        y_lab <- if_else(y %in% missing, NA_integer_, as.integer(y)) %>% add_labels(y)
        
        .data %>% mutate(`:=`(!!rlang::as_label(var1), x_lab), `:=`(!!rlang::as_label(var2), y_lab))
    }
    
}

collapse_chr <- function(.data, .var, collapse = ", ") {
    # Colapsar valores de una variable en un string.
    
    var_quo <- enquo(.var)
    
    pull(.data, !!var_quo) %>% str_c(., collapse = collapse)
}


