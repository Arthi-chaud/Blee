import * as yup from "yup";
import { Resource } from "./resource";
import { Image } from "./image";

const Package = Resource.concat(
    yup.object({
        name: yup.string().required(),
        slug: yup.string().required(),
        release_year: yup.date().required().nullable(),
        artist_id: yup.string().required().nullable(),
        artist_name: yup.string().required().nullable(),
        poster: Image.required().nullable(),
    }),
);

type Package = yup.InferType<typeof Package>;

type PackageSortingKeys = "name" | "addDate" | "artistName" | "release_date";

export { Package, type PackageSortingKeys };
