@echo off

echo "=== dotnet restore ==="
dotnet restore
if errorlevel 1 (
  exit /b %errorlevel%
)

echo "=== dotnet build %* ==="
dotnet build src/Anat %*
if errorlevel 1 (
  exit /b %errorlevel%
)

echo "=== dotnet test %* ==="
dotnet test tests/Anat.Tests %*
if errorlevel 1 (
  exit /b %errorlevel%
)

echo "=== dotnet pack %* ==="
dotnet pack src/Anat %*
