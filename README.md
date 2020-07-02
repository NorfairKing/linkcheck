# Link check

A fast linkchecker for CI.

This is designed to be run on CI to make sure that you do not have dead links on the website that you are about to deploy.

## Using linkcheck

Start your server locally on `localhost:8000` and run `linkcheck` as follows:

```
linkcheck http://localhost:8000
```

### Using linkcheck as part of a nix derivation

```
stdenv.mkDerivation {
  name = "site";
  buildInputs = [ final.haskellPackages.linkcheck final.killall ];
  buildCommand = ''
    mkdir -p $out
    cp -r ${site}/. $out

    $out/bin/site &
    linkcheck http://localhost:8000
    killall site
  '';
};
```

## Limitations

* `linkcheck` does not try to run JavaScript
* Only supports these tags:
  - `href` in `a` tags 
  - `src` in `img` tags.
  - `href` in `link` tags.
* Does not honor `robots.txt`
* Does not try to fetch external links
