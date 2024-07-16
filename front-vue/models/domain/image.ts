import * as yup from "yup";
import { Resource } from "./resource";

const Image = Resource.concat(
    yup.object({
        blurhash: yup.string().required(),
        aspect_ratio: yup.number().required(),
        colors: yup.array(yup.string().required()).required(),
    }),
);

type Image = yup.InferType<typeof Image>;

export { Image };
