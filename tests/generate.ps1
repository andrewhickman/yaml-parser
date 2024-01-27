$TestSuite = "$PSScriptRoot/../yaml-test-suite";

Push-Location $TestSuite
try
{
    Get-ChildItem "$TestSuite/**/in.yaml" -Recurse -Exclude tags | ForEach-Object {
        $Name = Split-Path -Parent (Resolve-Path -Path $_ -Relative)
        $TestName = "case_" + ($Name -replace '[/\\.]','_').Trim('_').ToLower()
        if (Test-Path (Join-Path (Split-Path -Parent $_.FullName) "error")) {
            "case!($TestName, r#`"$Name`"#, fail: true);"
        } else {
            "case!($TestName, r#`"$Name`"#);"
        }
    } | Set-Content "$PSScriptRoot/suite/cases.rs"
}
finally {
    Pop-Location
}