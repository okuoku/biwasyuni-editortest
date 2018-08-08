import { AppRegistry } from 'react-native';
import App from './App';
import startup from "./startup";

startup.startup();

AppRegistry.registerComponent('editortest', () => App);
