module Handler.Flexible_Table where

import Import

import StarExec.Types 
import Table.Data
import Table.Query
import Table.Get

getFlexible_TableR :: Query -> JobIds -> Handler Html
getFlexible_TableR q @ (Query ts) (JobIds ids) = do
  tab <- Table.Get.getManyJobCells ids
  defaultLayout $ do
    setTitle "Flexible Table"
    toWidget 
        [hamlet|
            our query: #{show q}
        |]
    display ts tab

-- display :: [Transform] -> Table -> Handler Html
display ts tab = do
  summary tab
  case ts of
    (t:ts') -> do
        [whamlet|
            must apply transformation #{show t}
        |]
        display ts $ apply t tab
        
    [] -> do
    -- no more transformers, display actual data
        let rs = rows tab
        [whamlet|
                <table class="table">
                    <tbody>
                      $forall row <- rs
                        <tr> 
                          $forall cell <- row
                            <td> #{tag cell}
            |]

summary tab = do
    [whamlet|
        summary: length tab = #{show (length (rows tab))}
    |]
    
apply t tab = tab
