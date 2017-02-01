import {Project} from '@atomist/rug/model/Core'
import {ProjectEditor} from '@atomist/rug/operations/ProjectEditor'
import {Microgrammar} from '@atomist/rug/tree/PathExpression'
import {PathExpressionEngine} from '@atomist/rug/tree/PathExpression'

class MgEditor implements ProjectEditor {
    name: string = "Constructed";
    description: string = "Uses single microgrammar";

    edit(project: Project) {
        let mg = new Microgrammar(
            'method',
            `public $type:§[A-Za-z0-9]+§ $fnName:§[A-Za-z0-9]+§ 
                (`);
        let eng: PathExpressionEngine = project.context().pathExpressionEngine().addType(mg);

        eng.with<any>(project, "//File()/method()/", n => {
            let currentName = n.fnName().value();
            let newName = currentName + n.type().value();
            n.fnName().update(newName)
        })
    }
}
export let editor = new MgEditor();
