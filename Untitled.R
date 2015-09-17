quality <-read.table("/Users/luoluowushengmimi/Documents/study/360-publicdata/quality.final.txt",header=TRUE,sep="",fill = TRUE)
rm(quality)
order_test <-read.table("/Users/luoluowushengmimi/Documents/study/360-publicdata/order_test_no_label.txt",header=TRUE,sep="",fill = TRUE)
product <-read.table("/Users/luoluowushengmimi/Documents/study/360-publicdata/product.final.txt",header=TRUE,sep="",fill = TRUE)
user <-read.table("/Users/luoluowushengmimi/Documents/study/360-publicdata/user.final.txt",header=TRUE,sep="",fill = TRUE)

quality <- subset(quality,select=-c(platform,mobile_verify,source,medium,is_paid,quality,mobile_source,mobile_medium,mobile_is_paid,mobile_quality,standard_type,guarantee_type,bank_id))

onlypvtest <- read.delim("~/Downloads/data/onlypvtest.txt", header=TRUE)
View(onlypvtest)
onlypvtest <-onlypvtest[-c(1,2,48)]
View(onlypvtest)
result <- onlypvtest$result;
fit.full <- glm(result ~ onlypvtest$product_type+onlypvtest$guarantee_type+onlypvtest$loan_term_min+onlypvtest$loan_term_max+onlypvtest$loan_term_type+onlypvtest$decision_cycle+onlypvtest$loan_cycle+onlypvtest$repayment_type+onlypvtest$loan_quota_min+onlypvtest$loan_quota_max+onlypvtest$interest_rate_type+onlypvtest$guarantee_required+onlypvtest$apply_num+onlypvtest$fangkuan_num+onlypvtest$house_register+onlypvtest$business_license+onlypvtest$legal_person+onlypvtest$married+onlypvtest$car+onlypvtest$car+onlypvtest$income+onlypvtest$house+onlypvtest$tax+onlypvtest$socialsecurity+onlypvtest$bank+onlypvtest$lifecost+onlypvtest$penalty+onlypvtest$term+onlypvtest$limit,data=onlypvtest,family=binomial())
onlypre <- read.delim("/Users/luoluowushengmimi/Documents/study/360-publicdata/data/test_onlypv.txt", header=TRUE)
onlypre <-onlypre[-c(1,2,48)]
> View(onlypvtest)
> onlypre <- read.delim("/Users/luoluowushengmimi/Documents/study/360-publicdata/data/test_onlypv.txt", header=TRUE)
> View(onlypre)
> onlypres <-onlypre[c(3,4,5,6,13,16,20,21,26,45)]
> View(onlypres)

> pvtrain <- read.delim("~/Downloads/data/pvtrain.txt", header=TRUE)
> pvtrain <-pvtrain[-c(1,2,48)]
> result <- pvtrain$result;
> fit.full <- glm(result ~ pvtrain$product_type+pvtrain$guarantee_type+pvtrain$loan_term_min+pvtrain$loan_term_max+pvtrain$interest_rate_type+pvtrain$apply_num+pvtrain$house_register+pvtrain$business_license+pvtrain$house+pvtrain$term ,data=pvtrain,family=binomial())
> 
  > summary(fit.full)
pre<-predict(fit.full, data.frame(onlypres))
> p<-exp(pre)/(1+exp(pre))
> View(p)
> result=1*(p>0.5)
write.csv(result, file="/Users/luoluowushengmimi/Documents/study/360-publicdata/data/result.csv")

fit.full <- glm(result ~ qualitytrain$application_type+qualitytrain$application_term+qualitytrain$application_limit+qualitytrain$op_type+qualitytrain$user_loan_experience+qualitytrain$user_has_car+qualitytrain$user_social_security+qualitytrain$business_license+qualitytrain$house+qualitytrain$term ,data=qualitytrain,family=binomial())

git.full <- glm(result ~ qualitytrain$product_type+ qualitytrain$guarantee_type+ qualitytrain$loan_term_min+ qualitytrain$loan_term_max+ qualitytrain$interest_rate_type+qualitytrain$loan_term_type+qualitytrain$decision_cycle +qualitytrain$loan_cycle + qualitytrain$repayment_type + qualitytrain $loan_quota_min + qualitytrain$loan_quota_max+ qualitytrain$interest_rate_type+ qualitytrain $guarantee_required+ qualitytrain$standard_type+ qualitytrain$apply_num+ qualitytrain$fangkuan_num+qualitytrain$is_p2p+qualitytrain$house_register+qualitytrain$business_license+qualitytrain$legal_person+qualitytrain$married+qualitytrain$car +qualitytrain$income +qualitytrain$house+qualitytrain$tax+ qualitytrain$socialsecurity+ qualitytrain$bank+qualitytrain$ lifecost+ qualitytrain$early_repayment+ qualitytrain$penalty+qualitytrain$application_type+qualitytrain$application_term+qualitytrain$application_limit+qualitytrain$op_type+qualitytrain$user_loan_experience+qualitytrain$user_has_car+qualitytrain$user_social_security+qualitytrain$user_income_by_card+qualitytrain$term+ qualitytrain $user_income_by_card+ qualitytrain$user_age+qualitytrain$col_type + qualitytrain$qid77 +qualitytrain$cash_receipts + qualitytrain$user_income_by_card + qualitytrain$user_work_period+ qualitytrain$col_value+ qualitytrain$com_op_period+ qualitytrain$com_month_flow+ qualitytrain$qid123+ qualitytrain$qid122+ qualitytrain$qid139+ qualitytrain$qid93+ qualitytrain$qid57+ qualitytrain$cash_settlement+ qualitytrain$user_nationality+ qualitytrain$qid145+ qualitytrain$house_payment_records+ qualitytrain$car_value,data=qualitytrain,family=binomial())

git.full <- glm(result ~ qualitytrain$loan_term_min+qualitytrain$decision_cycle+qualitytrain$loan_cycle+qualitytrain$apply_num+qualitytrain$fangkuan_num+qualitytrain$house_register+qualitytrain$car+qualitytrain$socialsecurity+qualitytrain$bank+qualitytrain$col_type+qualitytrain$qid77+qualitytrain$user_work_period,data=qualitytrain,family=binomial())
test_quality <- read.delim("/Users/luoluowushengmimi/Documents/study/360-publicdata/data/test_quality.txt", header=TRUE)
test_quality_sub <-test_quality[c(qualitytrain$loan_term_min,8,9,16,17,20,24,28,29,38,51,54)]
result_quality<-predict(git.full, data.frame(test_quality_sub))

