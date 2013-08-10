#!bin/usr/R
# Thomas J. Leeper
# Aarhus University
# April 15, 2013

# QuestionForm and AnswerKey Example Code for MTurkR

library(MTurkR)

# load QuestionForm and AnswerKey
QuestionForm <- paste0(scan(file="questionform_example.xml", what="character", sep="\n"), collapse="")
AnswerKey <- paste0(scan("answerkey_example.xml", what="character", sep="\n"), collapse="")

# validate QuestionForm
qfschema <- xmlSchemaParse("http://mechanicalturk.amazonaws.com/AWSMechanicalTurkDataSchemas/2005-10-01/QuestionForm.xsd")
validate1 <- xmlSchemaValidate(qfschema, QuestionForm)
# validate AnswerKey
akschema <- xmlSchemaParse("http://mechanicalturk.amazonaws.com/AWSMechanicalTurkDataSchemas/2005-10-01/AnswerKey.xsd")
validate2 <- xmlSchemaValidate(akschema, AnswerKey)

# create new QualificationType
newqual <- CreateQualificationType(name="A new coding test 2",
	description="Test of coding ability",
	status="Active",
	test.duration=seconds(hours=1),
	test=QuestionForm,
	answerkey=AnswerKey,
	validate.test=TRUE, # optional, since we did it above already
	validate.answerkey=TRUE, # optional, since we did it above already
	sandbox=TRUE)
