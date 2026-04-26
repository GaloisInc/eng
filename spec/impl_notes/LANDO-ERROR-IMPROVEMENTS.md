# Lando Parser Error Reporting Improvements

## Summary

Improved the Lando parser's error reporting to provide clear, actionable feedback instead of repetitive low-level backtracking messages.

## Problem

The original Lando parser used a DCG-style parser which works well for correct input, but produces poor error messages when parsing fails:

- **Repetitive output**: Shows the same error 5-10+ times due to backtracking
- **Low-level tokens**: Shows internal tokens like `token_s, w(Missing,13,12)` that are hard to interpret
- **No context**: Doesn't show the actual source code around the error
- **No guidance**: Provides no suggestions on how to fix the error
- **Signal buried in noise**: The real issue is buried in dozens of lines of backtracking noise

### Example OLD Output (27 lines):
```
ERROR: Parsing of Lando specification in test.lando failed

** Attempting "subsystem" [line 13], failing at: token_s, w(Missing,13,12), token_s
** Attempting "subsystem" [line 13], failing at: token_s, w(Missing,13,12)
** Attempting "subsystem" [line 13], failing at: token_s
** Wanted "specElement" but saw: w(subsystem,13,2)
** Wanted "specElement" but reached the end
[... repeats 20+ more times ...]
```

## Solution

Added intelligent error processing that:

1. **Tracks furthest progress**: Only reports errors from the deepest parse attempts (furthest into the input)
2. **Deduplicates**: Groups similar errors at the same location  
3. **Shows source context**: Displays the actual source lines around the error with highlighting
4. **Extracts readable info**: Converts internal tokens to human-readable text
5. **Provides suggestions**: Gives actionable advice for common errors

### Example NEW Output (concise and clear):
```
========== Parse Errors (showing most relevant) ==========

** Parse Error at line 13

  11 |   constraint The system shall work.
  12 | 
  13 |   subsystem Missing End
  14 | 
  15 | This should fail because of missing 'end' keyword
   Failed to parse: subsystem
   Near token: "Missing"
   >> Suggestion: A previous element may be incomplete - check for missing 'end' keyword or description paragraph

** Parse Error at line 13

  11 |   constraint The system shall work.
  12 | 
  13 |   subsystem Missing End
  14 | 
  15 | This should fail because of missing 'end' keyword
   Expected: specElement
   Found: "subsystem"
   >> Suggestion: Expected a spec element (system/subsystem/component/requirement) - previous element may be missing 'end'
===========================================================
```

## Implementation

Modified `src/datafmts/lando.pl`:

1. **Error Collection** (line 61): Store source filename for context display
2. **Error Processing** (line 79): Instead of dumping raw errors, call `process_and_show_errors/1`
3. **Smart Filtering** (lines 758-778): 
   - Parse error lines to extract structured info (line number, type, details)
   - Find furthest parse position
   - Keep only errors within 2 tokens of furthest position
   - Deduplicate by location and expected element
   - Limit to top 5 most informative errors

4. **Source Context** (lines 848-878): 
   - Read source file
   - Show 2 lines before and after error
   - Highlight error line in yellow

5. **Suggestions** (lines 884-904):
   - Pattern match on expected element type
   - Provide specific fix suggestions for common errors

## Benefits

✅ **27 lines → 2 relevant errors** (90%+ reduction in noise)  
✅ **Clear source context** with actual code lines  
✅ **Human-readable** tokens instead of internal representation  
✅ **Actionable suggestions** for fixing common mistakes  
✅ **Maintains parsing ability** - no changes to grammar, only error reporting  

## Testing

Tested with multiple error scenarios:

- **Missing 'end' keyword**: Correctly identifies location and suggests adding 'end'
- **Missing description**: Shows where description is required
- **Missing name**: Points to keyword that needs a name
- **Invalid syntax**: Shows exact location and what was expected

All test cases produce clear, concise error output with helpful suggestions.

## Compatibility

Changes are fully backwards compatible:
- No changes to the Lando grammar
- No changes to the parser structure or DCG rules
- Only changes how errors are displayed
- Preserves all the error information from the original saw/did_not_see predicates
