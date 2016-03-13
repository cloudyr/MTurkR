context("Account")

test_that("Account Balance Check", {
    expect_true(as.numeric(AccountBalance()$balance) == 10000)
    expect_true(is.numeric(SufficientFunds()$Total))
    expect_true(is.numeric(SufficientFunds(amount = 1, assignments = 1, hits = 1, bonus.ct = 1, bonus.amount = 1)$Total))
})

test_that("RequesterReport", {
    expect_true(is.data.frame(RequesterReport()))
    expect_true(is.data.frame(WorkerReport("A1RO9UJNWXMU65", period = "OneDay", sandbox = TRUE)))
})
test_that("RequesterReport", {
    expect_true(is.data.frame(WorkerReport("A1RO9UJNWXMU65", sandbox = TRUE)))
    expect_true(is.data.frame(WorkerReport("A1RO9UJNWXMU65", period = "OneDay", sandbox = TRUE)))
})


context("Workers")

test_that("GetBlockedWorkers", {
    expect_true(is.data.frame(GetBlockedWorkers(sandbox = TRUE)))
})
test_that("GrantBonus", {
    #expect_true(is.data.frame(GrantBonus("A1RO9UJNWXMU65", amount = 0.01, sandbox = TRUE)))
    expect_error(GrantBonus(workers = "A1RO9UJNWXMU65", sandbox = TRUE))
})
test_that("ContactWorkers", {
    expect_error(ContactWorkers(workers = "A1RO9UJNWXMU65", sandbox = TRUE))
    expect_error(ContactWorkers(subjects = "this is a test", sandbox = TRUE))
    expect_error(ContactWorkers(msgs = "this is a test", sandbox = TRUE))
    expect_true(is.data.frame(ContactWorkers("MTurkR Test", "This is a test", "A1RO9UJNWXMU65", sandbox = TRUE)))
})



context("HITs")

test_that("Question Formats", {
    expect_true(is.character(GenerateHITLayoutParameter("name", "value")))
    expect_true(inherits(GenerateExternalQuestion("https://www.example.com"), "ExternalQuestion"))
    f <- system.file("templates/htmlquestion1.xml", package = "MTurkR")
    expect_true(inherits(GenerateHTMLQuestion(file=f), "HTMLQuestion"))
})


r <- try(RegisterHITType(title = "Example HITType",
                     description = "empty",
                     reward = "0.01", 
                     duration = seconds(seconds = 30), 
                     keywords = "empty", 
                     sandbox = TRUE), silent = TRUE)

if (!inherits(r, "try-error")) {
    test_that("RegisterHITType", {
        expect_true(nrow(r) == 1)
    })

    test_that("SetHITTypeNotification", {
        n1 <- GenerateNotification("requester@example.com", event.type = "HITExpired")
        n2 <- GenerateNotification("https://sqs.us-east-1.amazonaws.com/123456789/Example", 
                                   transport = "SQS", 
                                   event.type = "HITExpired")
        expect_true(is.character(n1))
        expect_true(is.character(n2))
        s1 <- SetHITTypeNotification(hit.type = r$HITTypeId, notification = n1, active = TRUE, sandbox = TRUE)
        s2 <- SetHITTypeNotification(hit.type = r$HITTypeId, notification = n2, active = TRUE, sandbox = TRUE)
        expect_true(is.data.frame(s1))
        expect_true(is.data.frame(s2))
        s3 <- SetHITTypeNotification(hit.type = r$HITTypeId, notification = n1, active = FALSE, sandbox = TRUE)
        s4 <- SetHITTypeNotification(hit.type = r$HITTypeId, notification = n2, active = FALSE, sandbox = TRUE)
        expect_true(is.data.frame(s3))
        expect_true(is.data.frame(s4))
        rm(n1)
        rm(n2)
        rm(s1)
        rm(s2)
        rm(s3)
        rm(s4)
    })
}

h <- try(CreateHIT(hit.type = r$HITTypeId,
               question = GenerateExternalQuestion("https://www.example.com"),
               assignments = 1,
               expiration = seconds(seconds = 30), 
               sandbox = TRUE), silent = TRUE)

if (!inherits(h, "try-error")) {
    test_that("CreateHIT", {
        expect_true(nrow(h) == 1)
        expect_true(as.character(r$HITTypeId[1]) == as.character(h$HITTypeId[1]))
    })

    test_that("ExtendHIT (assignments)", {
        e1 <- ExtendHIT(hit = h$HITId, add.assignments = 1, sandbox = TRUE)
        expect_true(nrow(e1) == 1)
    })

    test_that("ExtendHIT (seconds)", {
        e1 <- ExtendHIT(hit = h$HITId, add.seconds = 3600, sandbox = TRUE)
        expect_true(nrow(e1) == 1)
    })

    test_that("ExpireHIT", {
        e2 <- ExpireHIT(hit = h$HITId, sandbox = TRUE)
        expect_true(nrow(e2) == 1)
    })

    test_that("ChangeHITType", {
        r2 <- RegisterHITType(title = "Example HITType 2",
                              description = "empty",
                              reward = "0.01", 
                              duration = seconds(seconds = 30), 
                              keywords = "empty", 
                              sandbox = TRUE)
        ch <- ChangeHITType(hit = h$HITId, new.hit.type = r2$HITTypeId, sandbox = TRUE)
        expect_true(nrow(ch) == 1)
    })

    test_that("SearchHITs", {
        expect_true(is.data.frame(SearchHITs(sandbox = TRUE)$HITs))
    })

    test_that("GetReviewableHITs", {
        expect_true(is.data.frame(GetReviewableHITs(sandbox = TRUE)))
    })

    test_that("GetAssignments", {
        a <- GetAssignments(hit = h$HITId, sandbox = TRUE)
        expect_true(nrow(a) == 0)
    })

    test_that("GetHIT", {
        expect_true(is.data.frame(GetHIT(hit = h$HITId, sandbox = TRUE)$HITs))
    })

    test_that("DisposeHIT", {
        d <- DisposeHIT(hit = h$HITId, sandbox = TRUE)
        expect_true(nrow(d) == 1)
    })
}


context("QualificationTypes")

sqt <- SearchQualificationTypes(return.all = TRUE, sandbox = TRUE)
if ("Example Qualification for Tests" %in% sqt$Name) {
    DisposeQualificationType(sqt$QualificationTypeId[which(sqt$Name == "Example Qualification for Tests")], sandbox = TRUE)
}
q1 <- try(CreateQualificationType(name = "Example Qualification for Tests",
                              description = "empty",
                              status = "Active",
                              keywords = "none", 
                              sandbox = TRUE), silent = TRUE)

if (!inherits(q1, "try-error")) {
    test_that("CreateQualificationType", {
        expect_true(nrow(q1) == 1)
    })

    test_that("GetHITsForQualificationType", {
        expect_true(is.data.frame(GetHITsForQualificationType(qual = q1$QualificationTypeId, sandbox = TRUE)$HITs))
        expect_error(GetHITsForQualificationType(sandbox = TRUE))
    })

    test_that("UpdateQualificationType", {
        u <- UpdateQualificationType(q1$QualificationTypeId, 
                                     description = "new", 
                                     sandbox = TRUE)
        expect_true(is.data.frame(u))
        g <- GetQualificationType(q1$QualificationTypeId, sandbox = TRUE)
        expect_true(g$Description[1] == "new")
        expect_error(GetQualificationType(sandbox = TRUE))
    })

    test_that("AssignQualification", {
        expect_true(is.data.frame(AssignQualification(q1$QualificationTypeId, workers = "A1RO9UJNWXMU65", sandbox = TRUE)))
    })

    test_that("UpdateQualificationScore", {
        expect_true(is.data.frame(UpdateQualificationScore(q1$QualificationTypeId, workers = "A1RO9UJNWXMU65", values = 2, sandbox = TRUE)))
        expect_true(is.data.frame(UpdateQualificationScore(q1$QualificationTypeId, workers = "A1RO9UJNWXMU65", increment = 1, sandbox = TRUE)))
    })

    test_that("RevokeQualification", {
        expect_true(is.data.frame(RevokeQualification(q1$QualificationTypeId, worker = "A1RO9UJNWXMU65", sandbox = TRUE)))
    })

    test_that("SearchQualificationTypes", {
        expect_true(is.data.frame(SearchQualificationTypes(sandbox = TRUE)))
    })

    test_that("DisposeQualificationType", {
        dis <- DisposeQualificationType(q1$QualificationTypeId, sandbox = TRUE)
        expect_true(nrow(dis) == 1)
        expect_error(DisposeQualificationType(sandbox = TRUE))
    })
}
