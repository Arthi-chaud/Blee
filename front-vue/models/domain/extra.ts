import * as yup from "yup";
import { Resource } from "./resource";
import { Image } from "./image";

const ExtraTypes = [
    "alternate_view",
    "backdrops",
    "behind_the_scenes",
    "interview",
    "music_video",
    "other",
    "performance",
    "trailer",
] as const;

type ExtraType = (typeof ExtraTypes)[number];

const Extra = Resource.concat(
    yup.object({
        name: yup.string().required(),
        thumbnail: Image.required().nullable(),
        package_id: yup.string().required(),
        artist_id: yup.string().required(),
        artist_name: yup.string().required(),
        file_id: yup.string().required(),
        disc_index: yup.number().required().nullable(),
        track_index: yup.number().required().nullable(),
        duration: yup.number().required(),
        type: yup
            .array(yup.mixed<ExtraType>().oneOf(ExtraTypes).required())
            .required(),
    }),
);

type Extra = yup.InferType<typeof Extra>;

type ExtraSortingKeys =
    | "name"
    | "artistName"
    | "packageName"
    | "addDate"
    | "releaseDate";

export { type ExtraType, type ExtraSortingKeys, Extra };
