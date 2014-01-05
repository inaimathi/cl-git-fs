# cl-git-fs 0.01
#### A git filesystem utility

`cl-git-fs` is a port of [this](http://hackage.haskell.org/package/filestore-0.3.2/docs/src/Data-FileStore-Git.html). Its goal is to allow the construction of file stores backed by [`git`](http://git-scm.com/) in Common Lisp.

### This library is still in development

- The external API is no longer likely to change (but still might)
- It currently only supports SBCL on Linux


### Docs

Specific methods are documented with docstrings; `describe` is your friend for more in-depth description.

### The External API

- As a rule, the `repo` argument is expected to be an absolute path string with a trailing slash (like `"/home/me/a-repo/"`, not `"/home/me/a-repo"` or `#p"/home/me/a-repo"`).
- Files within a repo are expected to be path strings relative to the repo root. For example, you would call `history` like this: `(history "/home/me/a-repo/" "file-name.txt")`
- Any functions that manipulate a repo take keyword arguments of `author`, `email` and `message`. They all have defaults, but if you want to be able to track edits by user (for example), you'll need to fill at least some of that in.

#### Repo modifying procedures

These procedures all edit the underlying repo in some way. So, you know, be careful. Other than initialize, they each take author/email/message keyword arguments that it uss for commit messages and metadata.

##### initialize!

Takes a path string, creates a directory there (if it doesn't already exist), then initializes and configures it as a git repo.

##### save-file!

Takes a repo path string, and a file-name string. If the specified file is either untracked, or modified, it adds and commits that file to the specified repo.

##### delete-file!

Takes a repo path string, and a file-name string. Deletes the specified file and commits the deletion.

##### move-file!

Takes a repo path string, an old file-path string and a new file-path string. Renames the specified file and commits the change.

#### Querying functions

These functions don't change the repo at all. As a result, they don't care about author metadata.

##### retrieve-file

Takes a repo path string, a file-path string and an optional revision-id string (it defaults to "HEAD", but can be any commit hash). If the file is an existing, tracked `blob` in the specified repository, it returns its contents at the specified revision. If you leave out the `revision-id`, it will give you the last comitted file contents. If you want the current contents of the specified file, just read it.

##### history

Takes a repo path string, a file-path string (or list of such strings) and the keyword arguments `since`, `until` and `limit`.

Returns a list of `(commit-hash timestamp author-name email raw-comment)` from the given repo relevant to the specified file(s), and restricted by the specified `since`/`until`/`limit` settings.

The returned `timestamp` is a Lisp `universal-time`, rather than a raw `UNIX` timestamp.

The `since` and `until` arguments are both in the appropriate git formats (see `man git-whatchanged` for some examples). The `limit` argument is a number.

##### latest

Takes a repo path string and a file-path string. Returns the `commit-hash` corresponding to the latest commit in the repo that affected the specified file. If the file is not tracked by the specified repo, returns `NIL` instead.

##### revision

Takes a repo path string and a commit-hash string. Returns `(commit-hash timestamp author-name email raw-comment)` for the specified commit.

##### index

Takes a repo path string. Returns a list of all tracked entries in the repo (an entry might be a `directory` or a `file`). Each entry is a path namestring relative from the repo root.

##### list-directory

Takes a repo path string and a directory path string. Returns a list of all tracked entries in the specified directory.

##### grep

Takes a repo path string and a search string (or list of same), as well as the keyword arguments `ignore-case?`, `match-all?` and `whole-words?`.

Returns a list of `(path-namestring line-number snippet)` of matches.

The `path-namestring` is a string designating the relative path to the matching file from the repo root. The `line-number` is the matching line, and the `snippet` is the contents of that line.

All three keyword arguments are booleans (what they do if active should be obvious).

##### ids-match?

Takes two ids, and determines if they match or not. It does this by checking if either id is the prefix of the other (this means that you should only pass it strings you know to be unique prefixes of relevant hashes, otherwise you might get false positives).

#### Internals of Interest

Checking existence of a file in history isn't as simple as checking whether it currently exists, or whether it had ever existed. In particular, it's possible for a file to have been deleted or renamed (in which case, it will not currently exist, but it will have `history` entries), and it's possible for a file to currently exist but be untracked (in which case it's on disk, but not in `history`). This is why `needs-saving?` is slightly more complicated than you'd think it should be at first.

TODO: document `needs-saving?`, `git-changed?` and some stuff from `util.lisp`.
