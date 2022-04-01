## Scrabble

In order to start the application make sure to connect to the NuGet server with the following command:

```
dotnet nuget add source https://nuget.pkg.github.com/jesper-bengtson/index.json -n FP2022 -u jesper-bengtson -p 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 --store-password-in-clear-text
```
If you've successfully connected you should get the following message: 

```Package source with Name: FP2022 added successfully.```

Now restart your IDE and use the following credentials to connect:

Username: `jesper-bengtson`

Password: `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`

If the command appears to fail please clear your local cache with the following command and retry:

```
dotnet nuget locals all --clear
```
To disconnect from your current connection use the following command:

```
dotnet nuget remove source FP2022
```

If successful, navigate to the ScrabbleGame folder where `Program.fs` is located and run `dotnet run`. That's it!