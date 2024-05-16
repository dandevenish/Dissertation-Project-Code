head(Index_rank)
cor.test(Index_rank$`Age60+wt`, Index_rank$`Index score normalised`,
         method = "spearman",
         exact = FALSE)

cor.test(Index_rank$`Bad health Wt`, Index_rank$`Index score normalised`,
         method = "spearman",
         exact = FALSE)

cor.test(Index_rank$`Green Blue space Wt`, Index_rank$`Index score normalised`,
         method = "spearman",
         exact = FALSE)

cor.test(Index_rank$`Deprivation score wt`, Index_rank$`Index score normalised`,
         method = "spearman",
         exact = FALSE)

cor.test(Index_rank$`Live births wt`, Index_rank$`Index score normalised`,
         method = "spearman",
         exact = FALSE)