import * as yup from "yup";
import { Resource } from "./resource";
import { Image } from "./image";

const Artist = Resource.concat(
    yup.object({
        name: yup.string().required(),
        slug: yup.string().required(),
        poster: Image.nullable(),
    }),
);

type Artist = yup.InferType<typeof Artist>;
type ArtistSortingKeys = "name" | "addDate";

export { Artist, type ArtistSortingKeys };
