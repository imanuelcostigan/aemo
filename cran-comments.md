## Test environments

GitHub Actions 

Build matrix:

```
- {os: windows-latest, r: '3.6'}
- {os: windows-latest, r: 'devel', args: "--no-manual"}
- {os: macOS-latest, r: '3.5'}
- {os: macOS-latest, r: '3.6'}
- {os: ubuntu-16.04, r: '3.6', rspm: "https://demo.rstudiopm.com/all/__linux__/xenial/latest"}
```

Have also checked on win-builder (release, devel).

## Check results

0 errors | 0 warnings | 0 note

## Reverse dependencies

There are no reverse dependencies

