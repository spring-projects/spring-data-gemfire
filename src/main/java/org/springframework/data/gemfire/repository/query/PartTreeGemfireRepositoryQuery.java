/*
 * Copyright 2012 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.springframework.data.gemfire.repository.query;

import java.util.Iterator;

import org.springframework.data.gemfire.GemfireTemplate;
import org.springframework.data.repository.query.ParametersParameterAccessor;
import org.springframework.data.repository.query.RepositoryQuery;
import org.springframework.data.repository.query.parser.Part;
import org.springframework.data.repository.query.parser.Part.Type;
import org.springframework.data.repository.query.parser.PartTree;

/**
 * {@link GemfireRepositoryQuery} backed by a {@link PartTree} and thus, deriving an OQL query from the backing query
 * method's name.
 * 
 * @author Oliver Gierke
 */
public class PartTreeGemfireRepositoryQuery extends GemfireRepositoryQuery {

	private final GemfireQueryMethod method;
	private final PartTree tree;
	private final GemfireTemplate template;

	/**
	 * Creates a new {@link PartTreeGemfireRepositoryQuery} using the given {@link GemfireQueryMethod} and
	 * {@link GemfireTemplate}.
	 * 
	 * @param method must not be {@literal null}.
	 * @param template must not be {@literal null}.
	 */
	public PartTreeGemfireRepositoryQuery(GemfireQueryMethod method, GemfireTemplate template) {

		super(method);

		Class<?> domainClass = method.getEntityInformation().getJavaType();

		this.tree = new PartTree(method.getName(), domainClass);
		this.method = method;
		this.template = template;
	}

	/* 
	 * (non-Javadoc)
	 * @see org.springframework.data.repository.query.RepositoryQuery#execute(java.lang.Object[])
	 */
	@Override
	public Object execute(Object[] parameters) {

		ParametersParameterAccessor parameterAccessor = new ParametersParameterAccessor(method.getParameters(), parameters);
		QueryString query = new GemfireQueryCreator(tree, method.getPersistentEntity()).createQuery(parameterAccessor
				.getSort());

		RepositoryQuery repositoryQuery = new StringBasedGemfireRepositoryQuery(query.toString(), method, template);

		return repositoryQuery.execute(prepareStringParameters(parameters));
	}

	private Object[] prepareStringParameters(Object[] parameters) {

		Iterator<Part> iterator = tree.getParts().iterator();
		Object[] result = new Object[parameters.length];

		for (int i = 0; i < parameters.length; i++) {
			Object parameter = parameters[i];

			if (parameter == null) {
				result[i] = parameter;
				continue;
			}

			Type type = iterator.next().getType();

			switch (type) {
			case CONTAINING:
				result[i] = String.format("%%%s%%", parameter.toString());
				break;
			case STARTING_WITH:
				result[i] = String.format("%s%%", parameter.toString());
				break;
			case ENDING_WITH:
				result[i] = String.format("%%%s", parameter.toString());
				break;
			default:
				result[i] = parameter;
			}
		}

		return result;
	}
}
