unit DTO.GitHUB.Release;

interface

uses
  DTO.GitHUB.ReleaseDTO, System.Generics.Collections, Json.DTO.Base;

type
  TRelease = TItemsDTO;

  TReleasesDTO = class(TReleaseDTO)
  strict protected
    property Items;
  published
    property Releases: TObjectList<TRelease> read GetItems;
  end;

implementation


end.
