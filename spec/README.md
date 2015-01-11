These files are the spec for both general Swift and Gulliver-specific extensions.

The swift/ directory contains swift files in directories based on the language guide chapters. All files present here
are expected to pass in both Gulliver and Apple Swift implementations.

The gulliver/ contains swift files in the same directory structure as the swift/ section. However these spec files are
specific to the Gulliver implementation. They deal with concepts specific to the JVM and Gulliver like interop.

Spec files all have a multiline comment at the top of the file. Inside this multiline comment, each section is
separated by sections whose titles are single lines with two dashes both prepended and appended. After the section
title all of the contents in lines after that leading up to the end of the multiline comment or the next section title
are the contents of that section. All \r's are removed. Example:

```swift
/*
--OUTPUT--
Hello World
*/
System.out.println("Hello World")
```

Currently the following sections are supported:

* `--OUTPUT--` - The expected stdout output. All \r's are removed.