Add-Type -TypeDefinition @"
using System;
using System.Runtime.InteropServices;

public class ProcessControl {
    [DllImport("ntdll.dll", SetLastError = true)]
    public static extern int NtSuspendProcess(IntPtr processHandle);

    [DllImport("kernel32.dll", SetLastError = true)]
    public static extern IntPtr OpenProcess(int access, bool inheritHandle, int processId);

    [DllImport("kernel32.dll", SetLastError = true)]
    public static extern bool CloseHandle(IntPtr hObject);
}
"@

function Suspend-Roblox {
    while ($true) {
        $proc = Get-Process -Name "RobloxPlayerBeta" -ErrorAction SilentlyContinue
        if ($proc) {
            $pid = $proc.Id
            $handle = [ProcessControl]::OpenProcess(0x0002, $false, $pid)
            if ($handle -ne [IntPtr]::Zero) {
                [ProcessControl]::NtSuspendProcess($handle) | Out-Null
                [ProcessControl]::CloseHandle($handle) | Out-Null
                Write-Host "Roblox suspended at $(Get-Date -Format 'HH:mm:ss')"
                break
            }
        }
        Start-Sleep -Milliseconds 50
    }
}

Suspend-Roblox
