export interface IUser {
  id: string;
  address: string;
  userType: string;
  userDID?: string;
  chats?: IChat[];
  createdAt: Date;
  lastLogin: Date;
}

export interface IChat {
  id: string;
  title: string;
  userId: string;
  user?: IUser;
  datasetUrl?: string;
  datasetDID?: string;
  datasetCredentialTxHash?: string;
  chatMessages?: IChatMessage[];
  createdAt: Date;
  updatedAt: Date;
}

export interface IChatMessage {
  id: string;
  chatId: string;
  chat?: IChat;
  role: string;
  content: string;
  createdAt: Date;
  updatedAt: Date;
}