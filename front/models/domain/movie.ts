import * as yup from "yup";
import { Resource } from "./resource";
import { Image } from "./image";

const MovieTypes = ["documentary", "concert"] as const;
type MovieType = (typeof MovieTypes)[number];

const Movie = Resource.concat(
    yup.object({
        name: yup.string().required(),
        slug: yup.string().required(),
        type: yup.mixed<MovieType>().oneOf(MovieTypes).required(),
        thumbnail: Image.required().nullable(),
        package_id: yup.string().required().nullable(),
        artist_id: yup.string().required().nullable(),
        artist_name: yup.string().required().nullable(),
        file_id: yup.string().required(),
    }),
);

type Movie = yup.InferType<typeof Movie>;

type MovieSortingKeys =
    | "name"
    | "artistName"
    | "packageName"
    | "addDate"
    | "releaseDate";

export { Movie, MovieTypes, type MovieType, type MovieSortingKeys };
