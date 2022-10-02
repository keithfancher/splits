module Options (cliOpts) where

import Options.Applicative
import Parse (ParseConf (..))

configParser :: Parser ParseConf
configParser =
  ParseConf
    <$> strOption
      ( long "sep"
          <> short 's'
          <> metavar "SEPARATOR"
          <> help "Character used to separate CSV fields"
          <> showDefault
          <> value ","
      )
    <*> option
      auto
      ( long "datecol"
          <> short 'd'
          <> help "Zero-indexed column number of date field"
          -- <> showDefault
          -- <> value 0
          <> metavar "INT"
      )
    <*> option
      auto
      ( long "amountcol"
          <> short 'a'
          <> help "Zero-indexed column number of amount field"
          -- <> showDefault
          -- <> value 1
          <> metavar "INT"
      )
    <*> option
      auto
      ( long "startrow"
          <> short 'r'
          <> help "Zero-indexed row number where the data starts (excluding header)"
          -- <> showDefault
          -- <> value 0
          <> metavar "INT"
      )

cliOpts :: ParserInfo ParseConf
cliOpts =
  info
    (configParser <**> helper)
    ( fullDesc
        <> progDesc "Requires some basic configs to parse the CSV data. The first CSV file will be Alice's expenses. The second will be Bob's. We'll use these names in the output to make things clearer, hopefully."
        <> header "Split shared expenses. Feed me two CSV files!"
    )
