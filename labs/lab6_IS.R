# beräkna IS
#IS= P(A,B)/sqrt(P(A)*P(B))

all_support<-support(rules,transactions = trans)

left<-lhs(rules)
left_support<-support(x = left,transactions = trans)
right<-rhs(rules)
right_support<-support(x = right,transactions = trans)

IS<-all_support/sqrt(left_support*right_support)
rules_df<-inspect(rules)
rules_df2<-cbind(rules_df,IS=IS)
rules_df2
# högre värden på IS är bra
rules_df2[order(rules_df2$IS,decreasing = TRUE),]