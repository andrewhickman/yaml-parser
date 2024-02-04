$TestSuite = "$PSScriptRoot/../yaml-test-suite";

Push-Location $TestSuite
try
{
    Get-ChildItem "$TestSuite/**/in.yaml" -Recurse -Exclude tags | ForEach-Object {
        $Name = ((Split-Path -Parent (Resolve-Path -Path $_ -Relative)) -replace '[/\\]','/').Trim('.').Trim('/')
        $TestName = "case_" + ($Name -replace '/','_').ToLower()

        if (Test-Path (Join-Path (Split-Path -Parent $_.FullName) "error")) {
            $FailArg = ", fail: true"
        } else {
            $FailArg = ""
        }

        "case!($TestName, `"$Name`"$SkipArg$FailArg);"
    } | Set-Content "$PSScriptRoot/suite/cases.gen.rs"
}
finally {
    Pop-Location
}