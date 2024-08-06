import * as yup from "yup";

export const FileInfo = yup.object({
    mimeCodec: yup.string().required().nullable(),
});

export type FileInfo = yup.InferType<typeof FileInfo>;
