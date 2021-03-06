{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module GitSpec (spec) where

import Test.Hspec
import Data.Time.LocalTime
import Data.Time.Calendar

import Str
import Git
import TestUtil

spec :: Spec
spec = do
    testUsualCommit
    testMerge
    testCommitWithoutMessage
    testMultipleCommits
    testMultipleCommitsFirstIsMerge
    testNoMessageUsualCommitWithCommitAfter
    testTag
    testTopDecorate
    testTopDecorateNoHead
    testMergeConflict

testMerge :: Spec
testMerge = it "parses merge commits" $ do
    let source = [strT|
        commit b1eec0f4d734432e434385ea83ee5852eaff1d7f
        Merge: xxxx
        Author:     David <t@a>
        AuthorDate: Mon Apr 8 18:50:43 2013 +0200
        Commit:     David <t@a>
        CommitDate: Mon Apr 8 18:50:43 2013 +0200

            Did merge.

        |]
    let expected = Commit {
        commitDate = LocalTime (fromGregorian 2013 4 8) (TimeOfDay 18 50 43),
        commitDesc = Just "Did merge.",
        commitFiles = [],
        commitAuthor = "David <t@a>",
        commitContents = "<pre></pre>",
        commitIsMerge = True,
        commitTags = [],
        commitIsCherryPicked = False
    }
    testParsecExpectFirst source parseCommits expected

testUsualCommit :: Spec
testUsualCommit = it "parses usual commits" $ do
    let source = [strT|
        commit b1eec0f4d734432e434385ea83ee5852eaff1d7f
        Author:     David <t@a>
        AuthorDate: Mon Apr 8 18:50:43 2013 +0200
        Commit:     David <t@a>
        CommitDate: Mon Apr 8 18:50:43 2013 +0200

            Did commit.

         test/src/main/users.js | 2 ++
         1 file changed, 2 insertions(+)

        |]
    let expected = Commit {
        commitDate = LocalTime (fromGregorian 2013 4 8) (TimeOfDay 18 50 43),
        commitDesc = Just "Did commit.",
        commitFiles = ["test/src/main/users.js"],
        commitAuthor = "David <t@a>",
        commitContents = "<pre>test/src/main/users.js | 2 ++</pre>",
        commitIsMerge = False,
        commitTags = [],
        commitIsCherryPicked = False
    }
    testParsecExpectFirst source parseCommits expected

testMergeConflict :: Spec
testMergeConflict = it "parses merge conflict" $ do
    -- this one is problematic with strT and trailing whitespace,
    -- which I drop on save => rather use a more traditional way.
    let source = "commit c4305424787636da6743221256aae93d53d0f642\n\
                 \Author: Emmanuel Touzery <et@gmail.com>\n\
                 \AuthorDate:   Mon May 19 17:03:36 2014 +0200\n\
                 \Commit: Emmanuel Touzery <et@gmail.com>\n\
                 \CommitDate:   Mon May 19 17:03:36 2014 +0200\n\n\
                 \    Merge branch 'master'\n    \n    Conflicts:\n\
                 \    generic/src/main/webapp/app/module/maintenance/route/route-ctrl.js\n\
                 \    generic/src/main/webapp/app/resources.js\n\n"
    let expected = Commit {
        commitDate = LocalTime (fromGregorian 2014 5 19) (TimeOfDay 17 03 36),
        commitDesc = Just "Merge branch 'master'\n    \n    Conflicts:\n    generic/src/main/webapp/app/module/maintenance/route/route-ctrl.js\n    generic/src/main/webapp/app/resources.js",
        commitFiles = [],
        commitAuthor = "Emmanuel Touzery <et@gmail.com>",
        commitContents = "<pre></pre>",
        commitIsMerge = False,
        commitTags = [],
        commitIsCherryPicked = False
    }
    testParsecExpectFirst source parseCommits expected

testMultipleCommits :: Spec
testMultipleCommits = it "parses multiple commits" $ do
    let source = [strT|
        commit b1eec0f4d734432e434385ea83ee5852eaff1d7f
        Author:     David <t@a>
        AuthorDate: Mon Apr 8 18:50:43 2013 +0200
        Commit:     David <t@a>
        CommitDate: Mon Apr 8 18:50:43 2013 +0200

            Did commit.

         test/src/main/users.js | 2 ++
         1 file changed, 2 insertions(+)

        commit b1eec0f4d734432e434385ea83ee5852eaff1d7f
        Author:     Emm <t@a>
        AuthorDate: Mon Apr 8 18:50:43 2013 +0200
        Commit:     Emm <t@a>
        CommitDate: Mon Apr 8 18:50:43 2013 +0200

            Did commit.

         test/src/main/users.js | 2 ++
         1 file changed, 2 insertions(+)

        |]
    let expected = [
            Commit {
                commitDate = LocalTime (fromGregorian 2013 4 8) (TimeOfDay 18 50 43),
                commitDesc = Just "Did commit.",
                commitFiles = ["test/src/main/users.js"],
                commitAuthor = "David <t@a>",
                commitContents = "<pre>test/src/main/users.js | 2 ++</pre>",
                commitIsMerge = False,
                commitTags = [],
                commitIsCherryPicked = False
            },
            Commit {
                commitDate = LocalTime (fromGregorian 2013 4 8) (TimeOfDay 18 50 43),
                commitDesc = Just "Did commit.",
                commitFiles = ["test/src/main/users.js"],
                commitAuthor = "Emm <t@a>",
                commitContents = "<pre>test/src/main/users.js | 2 ++</pre>",
                commitIsMerge = False,
                commitTags = [],
                commitIsCherryPicked = False
            }]
    testParsecExpectVal source parseCommits expected

testCommitWithoutMessage :: Spec
testCommitWithoutMessage = it "parses commits without message" $ do
    let source = [strT|
        commit b1eec0f4d734432e434385ea83ee5852eaff1d7f
        Author:     David <t@a>
        AuthorDate: Mon Apr 8 18:50:43 2013 +0200
        Commit:     David <t@a>
        CommitDate: Mon Apr 8 18:50:43 2013 +0200

         test/src/main/users.js | 2 ++
         1 file changed, 2 insertions(+)

        |]
    let expected = Commit {
        commitDate = LocalTime (fromGregorian 2013 4 8) (TimeOfDay 18 50 43),
        commitDesc = Nothing,
        commitFiles = ["test/src/main/users.js"],
        commitAuthor = "David <t@a>",
        commitContents = "<pre>test/src/main/users.js | 2 ++</pre>",
        commitIsMerge = False,
        commitTags = [],
        commitIsCherryPicked = False
    }
    testParsecExpectFirst source parseCommits expected


testMultipleCommitsFirstIsMerge :: Spec
testMultipleCommitsFirstIsMerge = it "parses multiple commits first is merge" $ do
    let source = [strT|
        commit d764a4398424f8bd2f1c659212ce6e0af83f5848
        Merge: dbcc720 95c25ed
        Author:     David B <david@b>
        AuthorDate: Wed Apr 3 16:54:39 2013 +0200
        Commit:     David B <david@b>
        CommitDate: Wed Apr 3 16:54:39 2013 +0200

            Merge branch 'master'

        commit 647d915f18d241185cf44b020ac5f962990350aa
        Author: Emmanuel Touzery <etouzery@gmail.com>
        AuthorDate:   Wed Apr 3 16:17:50 2013 +0200
        Commit: Emmanuel Touzery <etouzery@gmail.com>
        CommitDate:   Wed Apr 3 16:17:50 2013 +0200

            blabla

         t/README.md | 2 +-
         1 file changed, 1 insertion(+), 1 deletion(-)

        |]
    testParsecExpectVal source parseCommits [
        Commit {
            commitDate = LocalTime (fromGregorian 2013 4 3) (TimeOfDay 16 54 39),
            commitDesc = Just "Merge branch 'master'",
            commitFiles = [],
            commitAuthor = "David B <david@b>",
            commitContents = "<pre></pre>",
            commitIsMerge = True,
            commitTags = [],
            commitIsCherryPicked = False
        },
        Commit {
            commitDate = LocalTime (fromGregorian 2013 4 3) (TimeOfDay 16 17 50),
            commitDesc = Just "blabla",
            commitFiles = ["t/README.md"],
            commitAuthor = "Emmanuel Touzery <etouzery@gmail.com>",
            commitContents = "<pre>t/README.md | 2 +-</pre>",
            commitIsMerge = False,
            commitTags = [],
            commitIsCherryPicked = False
        }]

testNoMessageUsualCommitWithCommitAfter :: Spec
testNoMessageUsualCommitWithCommitAfter = it "parses no message usual commit with another commit after" $ do
    let source = [strT|
        commit b1eec434343dd42e95df8534233223121aff1d7f
        Author:     David <D@E>
        AuthorDate: Mon Apr 8 18:50:43 2013 +0200
        Commit:     David <D@E>
        CommitDate: Mon Apr 8 18:50:43 2013 +0200

         src/main/webapp/users.js | 2 ++
         1 file changed, 2 insertions(+)

        commit 8234434339d47233422582743434321ca25e2021
        Merge: f67c212 3fe1231
        Author:     da <da@gmail.com>
        AuthorDate: Mon Apr 8 18:44:14 2013 +0200
        Commit:     da <da@gmail.com>
        CommitDate: Mon Apr 8 18:44:14 2013 +0200

            Merge branch 'master'

        |]
    testParsecExpectVal source parseCommits [
        Commit {
              commitDate = LocalTime (fromGregorian 2013 4 8) (TimeOfDay 18 50 43),
              commitDesc = Nothing,
              commitFiles = ["src/main/webapp/users.js"],
              commitAuthor = "David <D@E>",
              commitContents = "<pre>src/main/webapp/users.js | 2 ++</pre>",
              commitIsMerge = False,
              commitTags = [],
              commitIsCherryPicked = False
        },
        Commit {
            commitDate = LocalTime (fromGregorian 2013 4 8) (TimeOfDay 18 44 14),
            commitDesc = Just "Merge branch 'master'",
            commitFiles = [],
            commitAuthor = "da <da@gmail.com>",
            commitContents = "<pre></pre>",
            commitIsMerge = True,
            commitTags = [],
            commitIsCherryPicked = False
        }]

testTopDecorate :: Spec
testTopDecorate = it "parses the top commit with decorate" $ do
    let source = [strT|
        commit 5caac9d1df215e71173a35b008b79dbe86050783 (HEAD, origin/master, origin/HEAD, master)
        Author:     Emmanuel Touzery <>
        AuthorDate: Fri Jan 10 15:13:28 2014 +0100
        Commit:     Emmanuel Touzery <>
        CommitDate: Fri Jan 10 15:13:28 2014 +0100

            that code really needed fixing

         .../project/folder/file.java   | 25 +++++++++++++---------
         1 file changed, 15 insertions(+), 10 deletions(-)

        |]
    testParsecExpectVal source parseCommits [
        Commit {
            commitDate = LocalTime (fromGregorian 2014 1 10) (TimeOfDay 15 13 28),
            commitDesc = Just "that code really needed fixing",
            commitFiles = [".../project/folder/file.java"],
            commitAuthor = "Emmanuel Touzery <>",
            commitContents = "<pre>.../project/folder/file.java   | 25 +++++++++++++---------</pre>",
            commitIsMerge = False,
            commitTags = [],
            commitIsCherryPicked = False
        }]

testTopDecorateNoHead :: Spec
testTopDecorateNoHead = it "parses the top commit with decorate (no head)" $ do
    let source = [strT|
        commit 5caac9d1df215e71173a35b008b79dbe86050783 (origin/tomee-port, tomee-port)
        Author:     Emmanuel Touzery <>
        AuthorDate: Fri Jan 10 15:13:28 2014 +0100
        Commit:     Emmanuel Touzery <>
        CommitDate: Fri Jan 10 15:13:28 2014 +0100

            that code really needed fixing

         .../project/folder/file.java   | 25 +++++++++++++---------
         1 file changed, 15 insertions(+), 10 deletions(-)

        |]
    testParsecExpectVal source parseCommits [
        Commit {
            commitDate = LocalTime (fromGregorian 2014 1 10) (TimeOfDay 15 13 28),
            commitDesc = Just "that code really needed fixing",
            commitFiles = [".../project/folder/file.java"],
            commitAuthor = "Emmanuel Touzery <>",
            commitContents = "<pre>.../project/folder/file.java   | 25 +++++++++++++---------</pre>",
            commitIsMerge = False,
            commitTags = [],
            commitIsCherryPicked = False
        }]

testTag :: Spec
testTag = it "creates an event for a tag" $ do
    let source= [strT|
        commit c903dc999b30f2f2c7bb657e3785bf335d953e42 (tag: v13.12.2a)
        Author:     Emmanuel Touzery <>
        AuthorDate: Wed Dec 11 21:11:07 2013 +0100
        Commit:     Emmanuel Touzery <>
        CommitDate: Wed Dec 11 21:11:07 2013 +0100

            last commit before release

         .../project/folder/file.java      | 58 +++++++++++-----------
         1 file changed, 29 insertions(+), 29 deletions(-)

        |]
    testParsecExpectVal source parseCommits [
        Commit {
            commitDate = LocalTime (fromGregorian 2013 12 11) (TimeOfDay 21 11 07),
            commitDesc = Just "last commit before release",
            commitFiles = [".../project/folder/file.java"],
            commitAuthor = "Emmanuel Touzery <>",
            commitContents = "<pre>.../project/folder/file.java      | 58 +++++++++++-----------</pre>",
            commitIsMerge = False,
            commitTags = ["v13.12.2a"],
            commitIsCherryPicked = False
        }]
