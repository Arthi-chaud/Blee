import * as yup from "yup";

const Resource = yup.object({
    id: yup.string().required(),
});

type Resource = yup.InferType<typeof Resource>;

export { Resource };
