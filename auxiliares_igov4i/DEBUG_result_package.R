#' @title Debug result
#' @author Luiz Paulo T.

# FUN package:

jud_package = Igov4i::aux_jud_clean(dataset_jud, depara = depara)

# Comparando com result plot_judiciario.Rmd {antigo Markdown}


# jud_rmd \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

jud_test = (jud_rmd == jud_package) %>%
            base::as.data.frame() %>% summary() %>% print()


# FUNÇÃO Igov4i::aux_jud_clean \\\\ APROVADA

# ===============================================================
jud_clean = Igov4i::jud_clean(db = dataset_jud, depara = depara)

jud_test = (bd_index_rm == jud_clean) %>%
            base::as.data.frame() %>% summary() %>% print()

# FUNÇÃO Igov4i::jud_clean \\\\ APROVADA

# =================================================================
# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
judiciario = Igov4i::igov_jud(df = jud_clean) # PACKAGE
indice_judiciario <- calc_new_judiciario(bd_index, peso = 1) # RM


jud_test = (judiciario == indice_judiciario) %>%
            base::as.data.frame() %>% summary() %>% print()

# ENCONTRADO DIFERENÇA
data_frame_dif = (judiciario == indice_judiciario) %>%
                  base::as.data.frame()










