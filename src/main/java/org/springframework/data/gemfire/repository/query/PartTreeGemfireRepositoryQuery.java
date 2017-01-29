/*
 * Copyright 2012-2018 the original author or authors.
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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.springframework.data.domain.Sort;
import org.springframework.data.gemfire.GemfireTemplate;
import org.springframework.data.repository.query.ParametersParameterAccessor;
import org.springframework.data.repository.query.RepositoryQuery;
import org.springframework.data.repository.query.parser.Part;
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

		QueryString query = new GemfireQueryCreator(tree, method.getPersistentEntity())
			.createQuery(parameterAccessor.getSort());

		RepositoryQuery repositoryQuery = new StringBasedGemfireRepositoryQuery(query.toString(), method, template);

		return repositoryQuery.execute(prepareStringParameters(parameters));
	}

	private Object[] prepareStringParameters(Object[] parameters) {
		Iterator<Part> partsIterator = tree.getParts().iterator();
		List<Object> stringParameters = new ArrayList<Object>(parameters.length);

		for (Object parameter : parameters) {
			if (parameter == null || parameter instanceof Sort) {
				stringParameters.add(parameter);
			}
			else {
				switch (partsIterator.next().getType()) {
					case CONTAINING:
						stringParameters.add(String.format("%%%s%%", parameter.toString()));
						break;
					case STARTING_WITH:
						stringParameters.add(String.format("%s%%", parameter.toString()));
						break;
					case ENDING_WITH:
						stringParameters.add(String.format("%%%s", parameter.toString()));
						break;
					default:
						stringParameters.add(parameter);
				}
			}
		}

		return stringParameters.toArray();
	}

}
