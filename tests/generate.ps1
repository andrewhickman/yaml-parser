Get-ChildItem "$PSScriptRoot/../yaml-test-suite/src/*" | ForEach-Object {
    "case!(case_$($_.BaseName.ToLower()), `"$($_.Name)`");"
} | Set-Content "$PSScriptRoot/cases.gen.rs"