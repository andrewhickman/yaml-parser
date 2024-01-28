$TestSuite = "$PSScriptRoot/../yaml-test-suite";

$Failing = @(
    "case_5kje",
    "case_5llu",
    "case_5tym",
    "case_87e4",
    "case_8udb",
    "case_9c9n",
    "case_9jba",
    "case_ctn5",
    "case_cvw2",
    "case_dbg4",
    "case_g4rs",
    "case_hre5",
    "case_j3bt",
    "case_l9u5",
    "case_lqz7",
    "case_m5dy",
    "case_nat4",
    "case_qlj7",
    "case_s98z",
    "case_sf5v",
    "case_udr7",
    "case_w9l4"
)

Push-Location $TestSuite
try
{
    Get-ChildItem "$TestSuite/**/in.yaml" -Recurse -Exclude tags | ForEach-Object {
        $Name = Split-Path -Parent (Resolve-Path -Path $_ -Relative)
        $TestName = "case_" + ($Name -replace '[/\\.]','_').Trim('_').ToLower()

        if ($TestName -in $Failing) {
            $SkipArg = ", skip: true"
        } else {
            $SkipArg = ""
        }
        if (Test-Path (Join-Path (Split-Path -Parent $_.FullName) "error")) {
            $FailArg = ", fail: true"
        } else {
            $FailArg = ""
        }

        "case!($TestName, r#`"$Name`"#$SkipArg$FailArg);"
    } | Set-Content "$PSScriptRoot/suite/cases.gen.rs"
}
finally {
    Pop-Location
}