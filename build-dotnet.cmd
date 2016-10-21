@echo off
cls

dotnet restore
if errorlevel 1 (
  exit /b %errorlevel%
)

dotnet test tests/Anat.Tests
if errorlevel 1 (
  exit /b %errorlevel%
)

dotnet pack src/Anat -c Release
