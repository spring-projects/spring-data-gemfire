/*
 * Copyright 2012-2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
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
import org.springframework.data.repository.query.parser.Part;
import org.springframework.data.repository.query.parser.PartTree;

/**
 * {@link GemfireRepositoryQuery} backed by a {@link PartTree}, deriving an OQL query
 * from the backing query method's name/signature.
 *
 * @author Oliver Gierke
 * @author John Blum
 * @see org.springframework.data.gemfire.repository.query.GemfireRepositoryQuery
 */
public class PartTreeGemfireRepositoryQuery extends GemfireRepositoryQuery {

	private final GemfireQueryMethod method;

	private final GemfireTemplate template;

	private final PartTree tree;

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

		this.method = method;
		this.template = template;
		this.tree = new PartTree(method.getName(), domainClass);
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.data.repository.query.RepositoryQuery#execute(java.lang.Object[])
	 */
	@Override
	public Object execute(Object[] arguments) {

		QueryString query = createQuery(this.method, this.tree, arguments);

		GemfireRepositoryQuery repositoryQuery = newRepositoryQuery(query, this.method, this.template);

		return repositoryQuery.execute(prepareStringParameters(arguments));
	}

	private QueryString createQuery(GemfireQueryMethod queryMethod, PartTree tree, Object[] arguments) {

		ParametersParameterAccessor parameterAccessor =
			new ParametersParameterAccessor(queryMethod.getParameters(), arguments);

		GemfireQueryCreator queryCreator = new GemfireQueryCreator(tree, queryMethod.getPersistentEntity());

		return queryCreator.createQuery(parameterAccessor.getSort());
	}

	private GemfireRepositoryQuery newRepositoryQuery(QueryString query,
			GemfireQueryMethod queryMethod, GemfireTemplate template) {

		GemfireRepositoryQuery repositoryQuery =
			new StringBasedGemfireRepositoryQuery(query.toString(), queryMethod, template);

		repositoryQuery.register(getQueryPostProcessor());

		return repositoryQuery;
	}

	private Object[] prepareStringParameters(Object[] parameters) {

		Iterator<Part> partsIterator = this.tree.getParts().iterator();

		List<Object> stringParameters = new ArrayList<>(parameters.length);

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
