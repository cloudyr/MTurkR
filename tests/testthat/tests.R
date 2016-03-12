context("Sandbox Tests")

test_that("Account Balance Check", {
    expect_true(as.numeric(AccountBalance()$balance) == 10000)
    expect_true(is.numeric(SufficientFunds()$Total))
})

test_that("RequesterReport", {
    expect_true(is.data.frame(RequesterReport()))
})
test_that("RequesterReport", {
    expect_true(is.data.frame(WorkerReport("A1RO9UJNWXMU65")))
})

r <- RegisterHITType(title = "Example HITType",
                     description = "empty",
                     reward = "0.01", 
                     duration = seconds(seconds = 30), 
                     keywords = "empty", 
                     sandbox = TRUE)
test_that("RegisterHITType", {
    expect_true(nrow(r) == 1)
})

h <- CreateHIT(hit.type = r$HITTypeId,
               question = GenerateExternalQuestion("https://www.example.com"),
               assignments = 1,
               expiration = seconds(seconds = 30), 
               sandbox = TRUE)
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
    ch <- ChangeHITType(hit = h$HITId, new.hit.type = r2$HITTypeId)
    expect_true(nrow(ch) == 1)
})

test_that("SearchHITs", {
    expect_true(is.data.frame(SearchHITs()$HITs))
})

test_that("GetAssignments", {
    a <- GetAssignments(hit = h$HITId, sandbox = TRUE)
    expect_true(nrow(a) == 0)
})

test_that("DisposeHIT", {
    d <- DisposeHIT(hit = h$HITId, sandbox = TRUE)
    expect_true(nrow(d) == 1)
})

q1 <- CreateQualificationType(name = "Example Qualification for Tests",
                              description = "empty",
                              status = "Active",
                              keywords = "none", 
                              sandbox = TRUE)
test_that("CreateQualificationType", {
    expect_true(nrow(q1) == 1)
})

test_that("UpdateQualificationType", {
    u <- UpdateQualificationType(q1$QualificationTypeId, 
                                 description = "new", 
                                 sandbox = TRUE)
    expect_true(nrow(u) == 1)
    g <- GetQualificationType(q1$QualificationTypeId, sandbox = TRUE)
    expect_true(g$Description[1] == "new")
})

test_that("DisposeQualificationType", {
    dis <- DisposeQualificationType(q1$QualificationTypeId, sandbox = TRUE)
    expect_true(nrow(dis) == 1)
})

test_that("SearchQualificationTypes", {
    expect_true(is.data.frame(SearchQualificationTypes()))
})
